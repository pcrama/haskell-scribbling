{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Transaction (
  ITransaction(..)
  , NonBlankText
  , pattern NonBlankText
  , TransactionEval(..)
  , evalForTransaction
  , mkNonBlankText
  ) where
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Char (isSpace)
import Control.Monad.Reader (MonadReader, asks, runReader)

class ITransaction a where
  account :: a -> T.Text
  date :: a -> Day
  otherAccount :: a -> Maybe NonBlankText
  otherName :: a -> Maybe NonBlankText
  description :: a -> Maybe NonBlankText
  amountCents :: a -> Int
  currency :: a -> T.Text

newtype NonBlankText = NonBlankText' { getNonBlankText :: T.Text }
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
  Select :: a -> (b -> Maybe a) -> TransactionEval b -> TransactionEval a
  Account :: TransactionEval T.Text

eval :: (ITransaction t, MonadReader t m) => TransactionEval a -> m a
eval (Constant a) = return a
eval (Select defaultValue lookupFun x) = do
  x' <- eval x
  return $ maybe defaultValue id $ lookupFun $ x'
eval Account = asks account

evalForTransaction :: ITransaction t => TransactionEval a -> t -> a
evalForTransaction prog = runReader (eval prog)
