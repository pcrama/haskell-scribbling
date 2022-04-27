{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module ConfigLanguage (
  Compiled(..),
  CompilerError,
  CompilerErrorMessage(..),
  ParsePosition,
  Value(..),
  ParsedValue,
  compile,
  parseConfigFileText,
  runValueParser
  ) where
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import Data.Char (isDigit, isSpace, isLetter)
import Data.Functor (void)
import qualified Data.Text as T
import Text.Parsec

import Transaction

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

pattern ProperList1 x e <- Cons x (Nil _) e
pattern ProperList2 x y e <- Cons x (Cons y (Nil _) _) e
pattern ProperList2' x y zs e <- Cons x (Cons y zs _) e
pattern ProperList3 x y z e <- Cons x (Cons y (Cons z (Nil _) _) _) e
pattern ProperList3' w x y zs e <- Cons w (Cons x (Cons y zs _) _) e
pattern ProperList4 w x y z e <- Cons w (Cons x (Cons y (Cons z (Nil _) _) _) _) e

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

data Compiled =
  AsBool (TransactionEval Bool)
  | AsText (TransactionEval T.Text)
  | AsPair Compiled Compiled
  deriving (Show)

type Env = String -> Maybe Compiled

-- | Run `MonadError e` action and always succeed returning the result in an Either
dropIntoEither :: MonadError e m => m a -> m (Either e a)
dropIntoEither action = (Right <$> action) `catchError` (pure . Left)

compile :: (MonadReader Env m, MonadError (CompilerError b) m, Show b)
        => Value b
        -> m Compiled
compile v = do
  asBool <- dropIntoEither $ compileAsBool v
  asText <- dropIntoEither $ compileAsText v
  asPair <- dropIntoEither asPairME
  case (asBool, asText, asPair) of
    (Left (TypeError {}, _), Left (TypeError {}, _), Right (p, q)) -> pure $ AsPair p q
    (Left (TypeError {}, _), Right t, Left (TypeError {}, _)) -> pure $ AsText t
    (Right b, Left (TypeError {}, _), Left (TypeError {}, _)) -> pure $ AsBool b
    (Left t, Left (TypeError {}, _), Left (TypeError {}, _)) -> throwError t
    (Left (TypeError {}, _), Left t, Left (TypeError {}, _)) -> throwError t
    (Left (TypeError {}, _), Left (TypeError {}, _), Left t) -> throwError t
    (Left _, Left t, Left _) -> throwError t
    (e, f, g) -> error $ "Bug in compiler implementation: types overlap" <> formatForError "b" e <> formatForError "t" f <> formatForError "p" g <> " @ "
  where formatForError s (Right _) = " " <> s <> ":R"
        formatForError s (Left e) = " " <> s <> ":" <> show (fst e) <> "@" <> show (snd e)
        asPairME = case v of
          ProperList3 (Sym "pair" _) fstSource sndSource _ -> do
            p <- compile fstSource
            q <- compile sndSource
            pure (p, q)
          Cons (Sym "pair" _) _ _ ->
            throwError (Msg "pair expects exactly 2 arguments", getTopExtraValue v)
          Sym s _ -> getSym s v $ \got -> assertPairFor ("pair variable " <> s) got v
          -- do
          --   env <- ask
          --   case env s of
          --     Nothing -> throwError (Msg $ "unknown symbol " <> s, getTopExtraValue v)
          --     Just got -> assertPairFor ("pair variable " <> s) got v
          _ -> throwError (TypeError "Not a proper pair" TEPair TEUnknown, getTopExtraValue v)

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
-- description ::Text

compileAsBool :: (MonadReader Env m, MonadError (CompilerError b) m, Show b)
              => Value b
              -> m (TransactionEval Bool)
compileAsBool v@(Str _ _) = throwError (TypeError "string literal" TEString TEBool, getTopExtraValue v)
compileAsBool (Nil _) = pure $ Constant False
compileAsBool v@(ProperList3 (Sym "pair" _) _ _ _) = throwError (TypeError "pair constructor" TEBool TEPair, getTopExtraValue v)
compileAsBool v@(Cons (Sym "pair" _) _ _) = throwError (Msg "pair takes exactly 2 parameters", getTopExtraValue v)
compileAsBool (ProperList2 (Sym "fst" _) pSource _) = do
  (p, _) <- compilePair pSource
  snd <$> assertBoolFor "fst" p pSource
compileAsBool v@(Cons (Sym "fst" _) _ _) = throwError (Msg "fst takes exactly one parameter", getTopExtraValue v)
compileAsBool (ProperList2 (Sym "snd" _) qSource _) = do
  (_, q) <- compilePair qSource
  snd <$> assertBoolFor "snd" q qSource
compileAsBool v@(Cons (Sym "snd" _) _ _) = throwError (Msg "snd takes exactly one parameter", getTopExtraValue v)
compileAsBool (ProperList3 (Sym "contains" _) haystackSource needleSource _) = do
  haystack <- compileAsText haystackSource
  needle <- compileAsText needleSource
  return $ haystack `ContainsCaseInsensitive` needle
compileAsBool v@(Cons (Sym "contains" _) _ _) = throwError (Msg "contains takes exactly 2 parameters"
                                                           , getTopExtraValue v)
compileAsBool (ProperList1 (Sym "and" _) _) = pure $ Constant True
compileAsBool (ProperList2 (Sym "and" _) x _) = compileAsBool x
compileAsBool (Cons (Sym "and" _) xs _) = foldValueAsProperList "list of AND terms must be a proper list"
                                                                step
                                                                (Constant True)
                                                                xs
  where step v (Constant True) = compileAsBool v
        step v (Constant False) = compileAsBool v >> pure (Constant False)
        step v t = compileAsBool v >>= (`elimLeftConst` t)
        elimLeftConst (Constant True) t = pure t
        elimLeftConst (Constant False) _ = pure $ Constant False
        elimLeftConst c t = pure $ And c t
compileAsBool (ProperList1 (Sym "or" _) _) = pure $ Constant True
compileAsBool (ProperList2 (Sym "or" _) x _) = compileAsBool x
compileAsBool (Cons (Sym "or" _) xs _) = foldValueAsProperList "list of OR terms must be a proper list"
                                                                step
                                                                (Constant False)
                                                                xs
  where step v (Constant False) = compileAsBool v
        step v (Constant True) = compileAsBool v >> pure (Constant True)
        step v t = compileAsBool v >>= (`elimLeftConst` t)
        elimLeftConst (Constant False) t = pure t
        elimLeftConst (Constant True) _ = pure $ Constant True
        elimLeftConst c t = pure $ Or c t
compileAsBool v@(ProperList2' (Sym "cond" _) defSource xs _) = do
  c <- compileCond defSource xs
  snd <$> assertBoolFor "cond" c v
compileAsBool v@(ProperList4 (Sym "lookup" _) _ _ _ _) =
  throwError (TypeError "lookup always returns a string" TEBool TEString, getTopExtraValue v)
compileAsBool v@(Cons (Sym "lookup" _) _ _) =
  throwError (Msg "lookup takes exactly 3 parameters", getTopExtraValue v)
compileAsBool v@(Sym s _) = getSym s v $ \got -> snd <$> assertBoolFor ("bool variable " <> s) got v
compileAsBool v@(Cons (Sym s _) _ _) = throwError (Msg $ "unknown function " <> s, getTopExtraValue v)
compileAsBool v = throwError (Msg "Unknown form in compileAsBool", getTopExtraValue v)

getSym :: (MonadReader Env m, MonadError (CompilerError z) m)
       => String -> Value z -> (Compiled -> m a) -> m a
getSym "t" _ f = f $ AsBool $ Constant True
getSym "nil" _ f = f $ AsBool $ Constant False
getSym "account" _ f = f $ AsText Account
getSym "other-account" _ f = f $ AsText OtherAccount
getSym "description" _ f = f $ AsText Description
getSym s v f = do
  env <- ask
  case env s of
    Nothing -> throwError (Msg $ "unknown symbol " <> s, getTopExtraValue v)
    Just got -> f got

compileAsText :: (MonadReader Env m, MonadError (CompilerError z) m, Show z)
              => Value z
              -> m (TransactionEval T.Text)
compileAsText v@Nil {} = throwError (TypeError "nil is not a string" TEBool TEString, getTopExtraValue v)
compileAsText (Str s _) = pure $ Constant $ T.pack s
compileAsText v@(Cons (Sym "contains" _) _ _) = throwError (TypeError "contains returns a bool" TEBool TEString
                                                           , getTopExtraValue v)
compileAsText v@(Cons (Sym "pair" _) _ _) = throwError (TypeError "pair constructor" TEPair TEString, getTopExtraValue v)
compileAsText v@(Cons (Sym "and" _) _ _) = throwError (TypeError "and" TEBool TEString, getTopExtraValue v)
compileAsText v@(Cons (Sym "or" _) _ _) = throwError (TypeError "or" TEBool TEString, getTopExtraValue v)
compileAsText (ProperList2 (Sym "fst" _) pairSource _) = do
  (p, _) <- compilePair pairSource
  snd <$> assertTextFor "fst" p pairSource
compileAsText (ProperList2 (Sym "snd" _) pairSource _) = do
  (_, q) <- compilePair pairSource
  snd <$> assertTextFor "snd" q pairSource
compileAsText (ProperList4 (Sym "lookup" _)
                           defaultValueSource
                           (Nil _)
                           _
                           _) = compileAsText defaultValueSource
compileAsText (ProperList4 (Sym "lookup" _)
                           defaultValueSource
                           listOfLists
                           keySource
                           _) = do
    lookupTableSource <- toListOfPairs listOfLists
    -- TODO: keys are always assumed to be of type String/Text
    key <- compileAsText keySource
    lookupTable <- traverse compileStrPair lookupTableSource
    defaultValue <- compileAsText defaultValueSource
    return $ Select defaultValue (`lookup` lookupTable) key
  where toListOfPairs = foldValueAsProperList "Improper list of key/lookup values" step []
        step (Nil _) [] = pure []
        step (ProperList2 x y _) ps = pure $ (x, y):ps
        step v _ = throwError (Msg "lookup expects a list of 2-element sublists"
                              , getTopExtraValue v)
        -- TODO: keys are always assumed to be of type String/Text
        compileStrPair (Str x _, Str y _) = pure (T.pack x, T.pack y)
        compileStrPair (Str _ _, yNonStr) = throwError (Msg "Expected literal string in lookup value"
                                                       , getTopExtraValue yNonStr)
        compileStrPair (xNonStr, Str _ _) = throwError (Msg "Expected literal string in lookup key"
                                                       , getTopExtraValue xNonStr)
        compileStrPair (xNonStr, _) = throwError (Msg "Expected literal string in lookup key+value"
                                                 , getTopExtraValue xNonStr)
compileAsText v@(ProperList2' (Sym "cond" _)
                              defaultValueSource
                              conditionsSource
                              _) = do
  c <- compileCond defaultValueSource conditionsSource
  snd <$> assertTextFor "cond" c v
compileAsText v@(Sym s _) = getSym s v $ \got -> snd <$> assertTextFor ("string variable " <> s) got v
  -- do
  -- env <- ask
  -- case env s of
  --   Nothing -> throwError (Msg $ "unknown symbol " <> s, getTopExtraValue v)
  --   Just got -> snd <$> assertTextFor ("string variable " <> s) got v
compileAsText v@(Cons (Sym s _) _ _) = throwError (Msg $ "unknown function " <> s, getTopExtraValue v)
compileAsText v = throwError (Msg "Unhandled case", getTopExtraValue v)

-- compileAsPair :: (MonadReader Env m, MonadError (CompilerError z) m, Show z)
--               => TEType -- (Value z -> m (TransactionEval a))
--               -> TEType -- (Value z -> m (TransactionEval b))
--               -> Value z
--               -> m Compiled -- (TransactionEval (a, b))
-- compileAsPair cmpFst cmpSnd (ProperList3 (Sym "pair" _)
--                                          fstSource
--                                          sndSource
--                                          _) = do
--     first <- compile fstSource
--     void $ typeCheckForPairComponent cmpFst first fstSource
--     second <- compile sndSource
--     void $ typeCheckForPairComponent cmpSnd second sndSource
--     pure $ AsPair first second
-- compileAsPair _ _ v@(Cons (Sym "pair" _) _ _) = throwError (Msg "pair expects exactly 2 arguments", getTopExtraValue v)
-- compileAsPair f s v@(Sym n _) = do
--   env <- ask
--   case env n of
--     Nothing -> throwError (Msg $ "unknown symbol " <> n, getTopExtraValue v)
--     Just got -> do
--       (p, q) <- assertPairFor "pair" got v
--       void $ typeCheckForPairComponent f p v
--       void $ typeCheckForPairComponent s q v
--       pure got
-- compileAsPair _ _ v = throwError (TypeError "Expected a pair" TEPair TEUnknown, getTopExtraValue v)

compilePair :: (MonadReader Env m, MonadError (CompilerError z) m, Show z)
            => Value z
            -> m (Compiled, Compiled)
compilePair (ProperList3 (Sym "pair" _) fstSource sndSource _) = do
    first <- compile fstSource
    second <- compile sndSource
    pure (first, second)
compilePair v@(Cons (Sym "pair" _) _ _) = throwError (Msg "pair expects exactly 2 arguments", getTopExtraValue v)
compilePair v@(Sym n _) = do
  env <- ask
  case env n of
    Nothing -> throwError (Msg $ "unknown symbol " <> n, getTopExtraValue v)
    Just got -> assertPairFor "pair" got v
compilePair v = throwError (TypeError "Expected a pair" TEPair TEUnknown, getTopExtraValue v)

assertTextFor :: MonadError (CompilerError z) m => String -> Compiled -> Value z -> m (Compiled, TransactionEval T.Text)
assertTextFor _ c@(AsText p) _ = pure (c, p)
assertTextFor s got v = throwError $ mkTypeError s TEString got v

assertBoolFor :: MonadError (CompilerError z) m => String -> Compiled -> Value z -> m (Compiled, TransactionEval Bool)
assertBoolFor _ c@(AsBool p) _ = pure (c, p)
assertBoolFor s got v = throwError $ mkTypeError s TEBool got v

assertPairFor :: MonadError (CompilerError z) m => String -> Compiled -> Value z -> m (Compiled, Compiled)
assertPairFor _ (AsPair p q) _ = pure (p, q)
assertPairFor s got v = throwError $ mkTypeError s TEPair got v

mkTypeError :: String
            -> TEType
            -> Compiled
            -> Value b
            -> CompilerError b
mkTypeError s wanted got v = (TypeError s wanted $ compiledToTEType got
                             , getTopExtraValue v)
  where compiledToTEType AsText {} = TEString
        compiledToTEType AsBool {} = TEBool
        compiledToTEType AsPair {} = TEPair

-- typeCheckForPairComponent :: MonadError (CompilerError z) m => TEType -> Compiled -> Value z -> m Compiled
-- typeCheckForPairComponent TEString c v = do -- (fst <$>) . assertTextFor "pair component"
--   void $ assertTextFor "pair component" c v
--   pure c
-- typeCheckForPairComponent TEBool c v = do -- (fst <$>) . assertBoolFor "pair component"
--   void $ assertBoolFor "pair component" c v
--   pure c
-- typeCheckForPairComponent TEPair c v = do
--   void $ assertPairFor "pair component" c v
--   pure c
-- typeCheckForPairComponent t _ _ = error $ "Unsupported type " <> show t <> " in typeCheckForPairComponent"

compileCond :: (MonadReader Env m, MonadError (CompilerError z) m, Show z)
  => Value z -- ^ default value to return if none of the clauses match
  -> Value z -- ^ proper list of (test result) 2-element sublists (test is always a bool)
  -> m Compiled
compileCond defaultValueSource conditionsSource = do
    defaultValue <- compile defaultValueSource
    case defaultValue of
      AsText d -> do
        conditions <- toListOfPairs compileAsText conditionsSource
        pure $ AsText $ Cond d conditions
      AsBool d -> do
        conditions <- toListOfPairs compileAsBool conditionsSource
        pure $ AsBool $ Cond d conditions
      AsPair {} -> throwError $ mkTypeError "unsupported result type for cond" TEString defaultValue defaultValueSource
  where toListOfPairs f = foldValueAsProperList "cond expects a list of conditions" (stepToListOfPairs_ f) []
        -- assertSameType _ AsText {} v@(AsText {}) = pure v
        -- assertSameType _ AsBool {} v@(AsBool {}) = pure v
        -- assertSameType src (AsPair dp dq) v@(AsPair rp rq) = do
        --   mapM_ (uncurry $ assertSameType src) [(dp, rp), (dq, rq)]
        --   pure v
        -- assertSameType src wanted got = throwError (TypeError "cond default/result type mismatch" (compiledToTEType wanted) $ compiledToTEType got, getTopExtraValue src)
        -- compiledToTEType AsText {} = TEString
        -- compiledToTEType AsBool {} = TEBool
        -- compiledToTEType AsPair {} = TEPair

stepToListOfPairs_ :: (MonadReader Env m, MonadError (CompilerError z) m, Show z)
     => (Value z -> m (TransactionEval a))
     -> Value z
     -> [(TransactionEval Bool, TransactionEval a)]
     -> m [(TransactionEval Bool, TransactionEval a)]
stepToListOfPairs_ _ (Nil _) [] = pure []
stepToListOfPairs_ compileResult (ProperList2 xSource ySource _) ps = do
  x <- compileAsBool xSource
  y <- compileResult ySource
  pure $ (x, y):ps
stepToListOfPairs_ _ v _ = throwError (Msg "Expected list of 2 elements for conditions"
                                      , getTopExtraValue v)

-- assertTypeFor :: MonadError (CompilerError z) m => String -> TEType -> Compiled -> Value z -> m (Compiled, TransactionEval a)
-- assertTypeFor _ TEString c@(AsText p) _ = pure (c, p)
-- assertTypeFor _ TEBool c@(AsBool p) _ = pure (c, p)
-- assertTypeFor _ TEPair c@(AsPair p q) _ = pure (c, (p, q))
-- assertTypeFor s TEUnknown _ v = error "Can't assertTypeFor " <> s <> " TEUnknown" <> show (getTopExtraValue v)
-- assertTypeFor s wanted got v = throwError (TypeError s wanted $ compiledToTEType got, getTopExtraValue v)
--   where compiledToTEType AsText {} = TEString
--         compiledToTEType AsBool {} = TEBool
--         compiledToTEType AsPair {} = TEPair
