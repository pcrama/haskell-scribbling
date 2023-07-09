{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Transaction
  ( ITransaction (..),
    LedgerEntry(..),
    NonBlankText,
    pattern NonBlankText,
    TransactionEval (..),
    evalForTransaction,
    mkNonBlankText,
    mkLedgerEntry,
    squeeze,
  )
where

import Control.Monad.Reader (MonadReader, asks, runReader)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Time.Calendar (Day)

class ITransaction a where
  account :: a -> T.Text
  date :: a -> Day
  otherAccount :: a -> Maybe NonBlankText
  otherName :: a -> Maybe NonBlankText
  description :: a -> Maybe NonBlankText
  amountCents :: a -> Int
  currency :: a -> T.Text
  identifyingComment :: a -> T.Text

squeeze :: T.Text -> T.Text
squeeze = T.unwords . T.words

newtype NonBlankText = NonBlankText' {getNonBlankText :: T.Text}
  deriving (Show, Eq)

pattern NonBlankText :: T.Text -> NonBlankText
pattern NonBlankText t <- (NonBlankText' t)

{-# COMPLETE NonBlankText #-}

mkNonBlankText :: T.Text -> Maybe NonBlankText
mkNonBlankText t
  | T.all isSpace t = Nothing
  | otherwise = Just $ NonBlankText' t

data TransactionEval a where
  Constant :: a -> TransactionEval a
  Select :: TransactionEval a -> (b -> Maybe a) -> TransactionEval b -> TransactionEval a
  Cond :: TransactionEval a -> [(TransactionEval Bool, TransactionEval a)] -> TransactionEval a
  ContainsCaseInsensitive :: TransactionEval T.Text -> TransactionEval T.Text -> TransactionEval Bool
  Pair :: TransactionEval a -> TransactionEval b -> TransactionEval (a, b)
  Fst :: TransactionEval (a, b) -> TransactionEval a
  Snd :: TransactionEval (a, b) -> TransactionEval b
  And :: TransactionEval Bool -> TransactionEval Bool -> TransactionEval Bool
  Or :: TransactionEval Bool -> TransactionEval Bool -> TransactionEval Bool
  Account :: TransactionEval T.Text
  OtherAccount :: TransactionEval T.Text -- `Nothing` becomes `T.empty`.
  Description :: TransactionEval T.Text

instance Show a => Show (TransactionEval a) where
  show (Constant a) = "(Constant " <> show a <> ")"
  show (Select d _ _) = "(Select " <> show d <> " _f _v)"
  show (Cond d xs) = "(Cond " <> show d <> " " <> foldMap show xs <> ")"
  show (ContainsCaseInsensitive hayStack needle) = "(contains " <> show hayStack <> " " <> show needle <> ")"
  show (Pair _ _) = "(Pair _ _)"
  show (Fst _) = "(Fst _)"
  show (Snd _) = "(Snd _)"
  show (And x y) = "(And "<> show x <> " " <> show y <> ")"
  show (Or x y) = "(Or "<> show x <> " " <> show y <> ")"
  show Account = "account"
  show OtherAccount = "other-account"
  show Description = "description"

eval :: (ITransaction t, MonadReader t m) => TransactionEval a -> m a
eval (Constant a) = return a
eval (Select defaultValue lookupFun x) = do
  x' <- eval x
  maybe (eval defaultValue) pure $ lookupFun x'
eval (Cond d conds) = foldr (\(test, expr) rest -> do
                                b <- eval test
                                if b then eval expr else rest)
                            (eval d)
                            conds
eval (ContainsCaseInsensitive mHayStack mNeedle) = do
  hayStack <- T.toLower <$> eval mHayStack
  needle <- T.toLower <$> eval mNeedle
  return $ not . null $ T.breakOnAll needle hayStack
eval (Pair mA mB) = do
  a <- eval mA
  b <- eval mB
  return (a, b)
eval (Fst ab) = fst <$> eval ab
eval (Snd ab) = snd <$> eval ab
eval (Or ma mb) = do
  a <- eval ma
  if a then return True else eval mb
eval (And ma mb) = do
  a <- eval ma
  if a then eval mb else return False
eval Account = asks account
eval OtherAccount = do
  oa <- asks otherAccount
  return $ case oa of
             Just (NonBlankText x) -> x
             Nothing -> T.empty
eval Description = do
  d <- asks description
  return $ maybe (T.pack "?") getNonBlankText d

evalForTransaction :: ITransaction t => TransactionEval a -> t -> a
evalForTransaction prog = runReader (eval prog)

data LedgerEntry = LedgerEntry
  { precedingComment :: T.Text,
    ledgerDate :: Day,
    ledgerText :: T.Text,
    ledgerAccount :: T.Text,
    ledgerAmountCents :: Int,
    ledgerCurrency :: T.Text,
    ledgerOtherAccount :: Maybe T.Text
  }

mkLedgerEntry ::
  ITransaction t =>
  -- | Program to extract the `ledgerText` from an `ITransaction` instance.
  TransactionEval T.Text ->
  -- | Program to extract the `ledgerAccount` from an `ITransaction` instance.
  TransactionEval T.Text ->
  -- | Program to extract the `ledgerOtherAccount` from an `ITransaction` instance.
  -- Blank values are interpreted as `Nothing`.
  TransactionEval T.Text ->
  -- | The transaction to be transformed into a `LedgerEntry`.
  t ->
  -- A `LedgerEntry` representing the transaction.
  LedgerEntry
mkLedgerEntry textProg accountProg otherProg t =
  LedgerEntry
    { precedingComment = identifyingComment t,
      ledgerDate = date t,
      ledgerText = evalForTransaction textProg t,
      ledgerAccount = evalForTransaction accountProg t,
      ledgerAmountCents = amountCents t,
      ledgerCurrency = currency t,
      ledgerOtherAccount =
        let other = evalForTransaction otherProg t
         in if T.all isSpace other then Nothing else Just other
    }
