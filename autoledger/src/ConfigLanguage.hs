{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module ConfigLanguage (
  Compiled(..),
  CompilerError,
  CompilerErrorMessage(..),
  ParsePosition,
  ParsedValue,
  TEType(..),
  Value(..),
  compile,
  parseConfigFileText,
  runValueParser
  ) where
import Control.Monad.Except (MonadError, throwError, liftEither)
import Control.Monad.Reader (MonadReader, ask)
import Data.Char (isDigit, isSpace, isLetter)
import Data.Functor (void)
import qualified Data.Text as T
import Text.Parsec

import Transaction
import Data.Bitraversable (bitraverse)

-- | A `Value`, the type parameter `e` is for extra information (e.g. source
-- location in a parsing context).
data Value e =
  Sym String e
  | Str String e
  | Cons (Value e) (Value e) e
  | Nil e
  deriving (Eq, Show, Functor)

type ParsePosition = (String, Int, Int)

type ParsedValue = Value ParsePosition

pattern ProperList2 :: Value e -> Value e -> e -> Value e
pattern ProperList2 x y e <- Cons x (Cons y (Nil _) _) e

pattern Apply :: String -> Value e -> Value e
pattern Apply x xs <- Cons (Sym x _) xs _

pattern Apply2 :: String -> Value e -> Value e
pattern Apply2 n p1 <- Cons (Sym n _) (Cons p1 (Nil _) _) _

pattern Apply2' :: String -> Value e -> Value e -> Value e
pattern Apply2' n p1 p2s <- Cons (Sym n _) (Cons p1 p2s _) _

pattern Apply3 :: String -> Value e -> Value e -> Value e
pattern Apply3 n p1 p2 <- Cons (Sym n _) (Cons p1 (Cons p2 (Nil _) _) _) _

pattern Apply4 :: String -> Value e -> Value e -> Value e -> Value e
pattern Apply4 n p1 p2 p3 <- Cons (Sym n _) (Cons p1 (Cons p2 (Cons p3 (Nil _) _) _) _) _

setTopExtraValue :: a -> Value a -> Value a
setTopExtraValue a (Sym s _) = Sym s a
setTopExtraValue a (Str s _) = Str s a
setTopExtraValue a (Cons x y _) = Cons x y a
setTopExtraValue a (Nil _) = Nil a

getTopExtraValue :: Value a -> a
getTopExtraValue (Sym _ a) = a
getTopExtraValue (Str _ a) = a
getTopExtraValue (Cons _ _ a) = a
getTopExtraValue (Nil a) = a

enrichWithParserState :: Monad m => (a -> ParsedValue) -> ValueParser m a -> ValueParser m ParsedValue
enrichWithParserState f pA = do
  pos <- getPosition
  setTopExtraValue (sourceName pos, sourceLine pos, sourceColumn pos) . f <$> pA

type ValueParsingState = ()
type ValueParser m a = ParsecT T.Text ValueParsingState m a

dummyParsingExtraValue :: (String, Int, Int)
dummyParsingExtraValue = ("dummy", 0, 0)

runValueParser :: SourceName
               -> T.Text
               -> Either ParseError ParsedValue
runValueParser = runParser parseValue ()

parseConfigFileText :: SourceName -> T.Text -> Either ParseError [ParsedValue]
parseConfigFileText = runParser implicitValueList ()
  where implicitValueList = whitespace *> (parseValue `sepBy` whitespace1) <* eof

whitespace :: Monad m => ValueParser m ()
whitespace = void $ many $ satisfy isSpace

whitespace1 :: Monad m => ValueParser m ()
whitespace1 = void $ many1 $ satisfy isSpace

parseListContent :: Monad m => ValueParser m ParsedValue
parseListContent = foldr (\x y -> Cons x y $ getTopExtraValue x) (Nil dummyParsingExtraValue)
  <$> (many (satisfy isSpace) *> many (parseValue <* many (satisfy isSpace)))

-- Parse a string: no escaping character, no new lines
parseStringContent :: Monad m => ValueParser m String
parseStringContent = many $ satisfy $ not . (`elem` ['\r', '\n', '"'])

-- Parse a symbol: start with a letter, then any number of digits, dashes (-), letters or underscores (_)
parseSymbolName :: Monad m => ValueParser m String
parseSymbolName = (:) <$> satisfy isLetter <*> many (satisfy isSymbolChar)
  where isSymbolChar c = isLetter c || c == '-' || isDigit c || c == '_'

parseValue :: Monad m => ValueParser m ParsedValue
parseValue =
  enrichWithParserState (const (Nil dummyParsingExtraValue)) (string "nil")
  <|> enrichWithParserState (`Str` dummyParsingExtraValue) (between (char '"') (char '"') parseStringContent)
  <|> enrichWithParserState (`Sym` dummyParsingExtraValue) parseSymbolName
  <|> enrichWithParserState id (between (char '(') (char ')') parseListContent)

-- | Type specification for error messages
data TEType = TEBool | TEString | TEPair | TEUnknown
  deriving (Show, Eq)

data CompilerErrorMessage =
  Msg String
  | TypeError String TEType TEType
  deriving (Show, Eq)

type CompilerError z = (CompilerErrorMessage, z)

foldValue :: MonadError e m => (Value a -> e) -> (Value a -> b -> m b) -> b -> Value a -> m b
foldValue _ _ d (Nil _) = pure d
foldValue e f d (Cons x xs _) = foldValue e f d xs >>= f x
foldValue e _ _ v = throwError $ e v

foldValueAsProperList :: MonadError (CompilerError z) m => String -> (Value z -> b -> m b) -> b -> Value z -> m b
foldValueAsProperList s = foldValue $ \v -> (Msg s, getTopExtraValue v)

mapValueAsProperList :: MonadError (CompilerError z) m => String -> (Value z -> m b) -> Value z -> m [b]
mapValueAsProperList s f = foldValue (\v -> (Msg s, getTopExtraValue v)) step []
  where step x xs = (:xs) <$> f x

assertListOfListOf2 :: MonadError (CompilerError b) m
  => String
  -> Value b
  -> m [(Value b, Value b)]
assertListOfListOf2 err = \case
  Nil {} -> pure []
  xs@Cons {} -> mapValueAsProperList err isListOf2 xs
  v -> throwError (Msg $ err <> " must be a list of 2-element sublists"
                  , getTopExtraValue v)
  where isListOf2 (ProperList2 a b _) = pure (a, b)
        isListOf2 v = throwError (Msg $ err <> " must be lists of exactly 2 values"
                                 , getTopExtraValue v)

data Compiled z =
  AsBool z (TransactionEval Bool)
  | AsText z (TransactionEval T.Text)
  | AsTextPair z (TransactionEval (T.Text, T.Text))
  deriving (Show)

type Env z = String -> Maybe (Compiled z)

-- "literal constant"
-- (lookup <def> ((<key1> <result1>)...) <key>)
-- (cond <def> (<bool1> <result1>)...)
-- (contains <haystack :: Text> <needle :: Text>) :: Bool
-- (pair <fst> <snd>)
-- (fst <pair>)
-- (snd <pair>)
-- (and <bool1>...) :: Bool
-- (or <bool1>...) :: Bool
-- account :: Text
-- other-account :: Text
-- description :: Text

compile :: (MonadReader (Env b) m, MonadError (CompilerError b) m)
        => Value b
        -> m (Compiled b)
compile v@(Sym s _) = getSym s (getTopExtraValue v) pure
compile v@(Str s _) = pure $ AsText (getTopExtraValue v) $ Constant $ T.pack s
compile v@(Apply4 "lookup" d l k) = compileLookup d l k $ getTopExtraValue v
compile v@(Apply "lookup" _) = invalidApply "lookup" "exactly 3" v
compile (Apply2' "cond" d cs) = compileCond d cs
compile v@(Apply "cond" _) = invalidApply "cond" "2 or more" v
compile v@(Apply3 "contains" h n) =
  AsBool (getTopExtraValue v) . uncurry ContainsCaseInsensitive <$> compileContains h n
compile v@(Apply "contains" _) = invalidApply "contains" "exactly 2" v
compile v@(Apply3 "pair" x y) = AsTextPair (getTopExtraValue v) <$> compilePair x y
compile v@(Apply "pair" _) = invalidApply "pair" "exactly 2" v
compile (Apply2 "fst" p) = compilePairAccess Fst p
compile v@(Apply "fst" _) = invalidApply "fst" "exactly 1" v
compile (Apply2 "snd" p) = compilePairAccess Snd p
compile v@(Apply "snd" _) = invalidApply "snd" "exactly 1" v
compile v@(Apply "and" bs) = AsBool (getTopExtraValue v) <$> compileBoolAndOrOp And "and" True bs
compile v@(Apply "or" bs) = AsBool (getTopExtraValue v)  <$> compileBoolAndOrOp Or "or" False bs
compile v@(Apply s _) = throwError (Msg $ "Unknown function " <> s
                                   , getTopExtraValue v)
compile v@(Nil _) = pure $ AsBool (getTopExtraValue v) $ Constant False
compile v = throwError (Msg "I can't compile this", getTopExtraValue v)

compileLookup :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => Value b
  -> Value b
  -> Value b
  -> b
  -> m (Compiled b)
compileLookup defaultSource xs keySource topExtraValue = do
    def <- compile defaultSource
    xsList <- assertListOfListOf2 "lookup key/value pairs" xs
    case xsList of
      [] -> pure def
      _ -> compileLookup1 xsList keySource topExtraValue def

compileLookup1 :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => [(Value b, Value b)] -- ^ key value pair source
  -> Value b -- ^ key source
  -> b -- ^ error information of complete lookup form
  -> Compiled b -- ^ Compiled default value
  -> m (Compiled b)
compileLookup1 xs keySource topExtraValue = \case
  AsBool p d -> handleBool p d
  AsText p d -> handleText p d
  AsTextPair p pq -> handlePair p pq
  where handleBool p d = traverse (bitraverse pure (asBool "lookup result")) xs
                         >>= compileLookup2 d keySource (AsBool p) topExtraValue
        handleText p d = traverse (bitraverse pure (asText "lookup result")) xs
                         >>= compileLookup2 d keySource (AsText p) topExtraValue
        handlePair p d = traverse (bitraverse pure (asTextPair "lookup result")) xs
                         >>= compileLookup2 d keySource (AsTextPair p) topExtraValue

compileLookup2 :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => TransactionEval r -- ^ compiled default value to return from lookup
  -> Value b -- ^ uncompiled key to query
  -> (TransactionEval r -> Compiled b) -- ^ wrap typed TransactionEval with matching data-level tag
  -> b -- For error messages
  -> [(Value b, TransactionEval r)] -- ^ uncompiled key paired with already compiled matching result
  -> m (Compiled b)
compileLookup2 d keySource wrap topExtraValue kSource = do
    key <- compile keySource
    case key of
      AsBool _ k -> traverse (repack asBool) kSource
                    >>= compileLookup3 d k wrap topExtraValue
      AsText _ k -> traverse (repack asText) kSource
                    >>= compileLookup3 d k wrap topExtraValue
      AsTextPair {} -> throwError (Msg "key values of type pair not supported for lookup"
                                  , getTopExtraValue keySource)
  where repack asX (v, r) = do
          c <- asX "lookup key" v
          pure (c , r)

compileLookup3 :: (MonadError (CompilerError b) m, Eq k)
  => TransactionEval a
  -> TransactionEval k
  -> (TransactionEval a -> Compiled b) -- ^ wrap typed TransactionEval with matching data-level tag
  -> b
  -> [(TransactionEval k, TransactionEval a)]
  -> m (Compiled b)
compileLookup3 d k wrap b kvs = do
    kvs' <- traverse isAllConstants kvs
    pure $ wrap $ Select d (`lookup` kvs') k
  where isAllConstants (Constant kc, Constant vc) = pure (kc, vc)
        isAllConstants _ = throwError (Msg "Only handling constant keys/values at the moment", b)

compileCond :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => Value b
  -> Value b
  -> m (Compiled b)
compileCond defaultSource xs = do
  def <- compile defaultSource
  xsList <- assertListOfListOf2 "cond condition/result pairs" xs
  case xsList of
    [] -> pure def
    _ -> compileCond1 xsList def

compileCond1 :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => [(Value b, Value b)]
  -> Compiled b
  -> m (Compiled b)
compileCond1 xs = \case
  AsBool p b -> do
    traverse compileClause xs >>= compileCond2 b (AsBool p) (getAsBool errMsg)
  AsText p t -> do
    traverse compileClause xs >>= compileCond2 t (AsText p) (getAsText errMsg)
  AsTextPair p tt ->
    traverse compileClause xs >>= compileCond2 tt (AsTextPair p) (getAsTextPair errMsg)
  where errMsg = "result in a cond clause"
        compileClause (cond, res) = do
          condC <- asBool "condition inside a cond" cond
          resC <- compile res
          pure (condC, resC)

compileCond2 :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => TransactionEval r
  -> (TransactionEval r -> Compiled b)
  -> (Compiled b -> Either (CompilerError b) (TransactionEval r))
  -> [(TransactionEval Bool, Compiled b)]
  -> m (Compiled b)
compileCond2 def wrapResultType decomposeIntoResultType conditions =
  compileCond3 decomposeIntoResultType conditions >>= compileCond4 def wrapResultType

compileCond3 :: (MonadError (CompilerError b) m, MonadReader (Env b) m)
  => (Compiled b -> Either (CompilerError b) (TransactionEval r))
  -> [(TransactionEval Bool, Compiled b)]
  -> m [(TransactionEval Bool, TransactionEval r)]
compileCond3 decomposeIntoResultType = traverse decomposer
  where decomposer (cond, cmpled) = (cond,) <$> liftEither (decomposeIntoResultType cmpled)

compileCond4 :: Monad m
  => TransactionEval r
  -> (TransactionEval r -> Compiled b)
  -> [(TransactionEval Bool, TransactionEval r)]
  -> m (Compiled b)
compileCond4 def wrapResultType xs = pure $ compileCondAssemble wrapResultType def xs

compileCondAssemble ::
  (TransactionEval r -> Compiled b)
  -> TransactionEval r
  -> [(TransactionEval Bool, TransactionEval r)]
  -> Compiled b
compileCondAssemble ctor d cs = ctor $ Cond d cs

getAsBool :: String -> Compiled b -> Either (CompilerError b) (TransactionEval Bool)
getAsBool _ (AsBool _ b) = pure b
getAsBool err x = Left $ mkTypeError err TEBool x

getAsText :: String -> Compiled b -> Either (CompilerError b) (TransactionEval T.Text)
getAsText _ (AsText _ b) = pure b
getAsText err x = Left $ mkTypeError err TEString x

getAsTextPair :: String -> Compiled b -> Either (CompilerError b) (TransactionEval (T.Text, T.Text))
getAsTextPair _ (AsTextPair _ b) = pure b
getAsTextPair err x = Left $ mkTypeError err TEPair x

asBool :: (MonadReader (Env b) m, MonadError (CompilerError b) m) => String -> Value b -> m (TransactionEval Bool)
asBool err s = do
  compiled <- compile s
  snd <$> assertBoolFor err compiled

asText :: (MonadReader (Env b) m, MonadError (CompilerError b) m) => String -> Value b -> m (TransactionEval T.Text)
asText err s = do
  compiled <- compile s
  snd <$> assertTextFor err compiled

asTextPair :: (MonadReader (Env b) m, MonadError (CompilerError b) m) => String -> Value b -> m (TransactionEval (T.Text, T.Text))
asTextPair err s = do
  compiled <- compile s
  snd <$> assertTextPairFor err compiled

compileBoolAndOrOp :: (MonadError (CompilerError b) m, MonadReader (Env b) m) =>
  (TransactionEval Bool -> TransactionEval Bool -> TransactionEval Bool)
  -> String
  -> Bool
  -> Value b
  -> m (TransactionEval Bool)
compileBoolAndOrOp op opName defaultValue =
  foldValueAsProperList opName step (Constant defaultValue)
  where step v o@(Constant b)
          | b == defaultValue = asBool (opName <> " operand") v
          | otherwise = asBool (opName <> " operand") v >> pure o
        step v o = do
          c <- asBool (opName <> " operand") v
          case c of
            Constant b | b == defaultValue -> pure o
                       | otherwise -> pure c
            _ -> pure $ op c o

compileContains :: (MonadError (CompilerError b) m, MonadReader (Env b) m) =>
  Value b -> Value b -> m (TransactionEval T.Text, TransactionEval T.Text)
compileContains haystackSource needleSource =
  (,) <$> asText "haystack" haystackSource <*> asText "needle" needleSource

compilePair :: (MonadError (CompilerError b) m, MonadReader (Env b) m) =>
  Value b -> Value b -> m (TransactionEval (T.Text, T.Text))
compilePair fstSource sndSource =
    mkPair <$> asText "pair: fst" fstSource <*> asText "pair: snd" sndSource
  where mkPair (Constant p) (Constant q) = Constant (p, q)
        mkPair x y = Pair x y

compilePairAccess :: (MonadError (CompilerError b) m, MonadReader (Env b) m) =>
  (TransactionEval (T.Text, T.Text) -> TransactionEval T.Text) -> Value b -> m (Compiled b)
compilePairAccess extractor source = do
  pair <- asTextPair "accessing a pair's component" source
  pure $ AsText (getTopExtraValue source) $ extractor pair

invalidApply :: (MonadError (CompilerError b) m) =>
  String -> String -> Value b -> m a
invalidApply s n v = throwError (Msg $ s <> " takes " <> n <> " parameters"
                                , getTopExtraValue v)

getSym :: (MonadReader (Env b) m, MonadError (CompilerError b) m)
       => String -> b -> (Compiled b -> m a) -> m a
getSym "t" p f = f $ AsBool p $ Constant True
getSym "nil" p f = f $ AsBool p $ Constant False
getSym "account" p f = f $ AsText p Account
getSym "other-account" p f = f $ AsText p OtherAccount
getSym "description" p f = f $ AsText p Description
getSym s v f = do
  env <- ask
  case env s of
    Nothing -> throwError (Msg $ "unknown symbol " <> s, v)
    Just got -> f got

assertTextFor :: MonadError (CompilerError z) m => String -> Compiled z -> m (Compiled z, TransactionEval T.Text)
assertTextFor _ c@(AsText _ p) = pure (c, p)
assertTextFor s got = throwError $ mkTypeError s TEString got

assertBoolFor :: MonadError (CompilerError z) m => String -> Compiled z -> m (Compiled z, TransactionEval Bool)
assertBoolFor _ c@(AsBool _ p) = pure (c, p)
assertBoolFor s got = throwError $ mkTypeError s TEBool got

assertTextPairFor :: MonadError (CompilerError z) m => String -> Compiled z -> m (Compiled z, TransactionEval (T.Text, T.Text))
assertTextPairFor _ v@(AsTextPair _ q) = pure (v, q)
assertTextPairFor s got = throwError $ mkTypeError s TEPair got

-- | Create a type error to `throwError` in the `MonadError (CompilerError b) m` monad
mkTypeError :: String -- ^ error message
            -> TEType -- ^ desired type
            -> Compiled b -- ^ compiled value that was actually seen
            -> CompilerError b
mkTypeError s wanted got = (TypeError s wanted teType, v)
  where (v, teType) = compiledToTEType got
        compiledToTEType (AsText b _) = (b, TEString)
        compiledToTEType (AsBool b _) = (b, TEBool)
        compiledToTEType (AsTextPair b _) = (b, TEPair)
