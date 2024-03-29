module TransactionTest (
  transactionSpecs,
  Transaction(..)
  ) where

import           Test.Hspec
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)

import           Lib

data Transaction = Transaction {
  _account :: T.Text
  , _date :: Day
  , _otherAccount :: Maybe NonBlankText
  , _otherName :: Maybe NonBlankText
  , _description :: Maybe NonBlankText
  , _amountCents :: Int
  , _currency :: T.Text
  , _identifyingComment :: T.Text
  } deriving (Show, Eq)

instance ITransaction Transaction where
  account = _account
  date = _date
  otherAccount = _otherAccount
  otherName = _otherName
  description = _description
  amountCents = _amountCents
  currency = _currency
  identifyingComment = _identifyingComment

dummyTransaction :: Transaction
dummyTransaction = Transaction {
  _account = "account"
  , _date = fromGregorian 2022 3 30
  , _otherAccount = mkNonBlankText "other account"
  , _otherName = mkNonBlankText "other name"
  , _description = mkNonBlankText "description"
  , _amountCents = 1
  , _currency = "EUR"
  , _identifyingComment = "Dummy identifying comment"
  }

transactionSpecs :: SpecWith ()
transactionSpecs = describe "src/Transaction" $ do
  describe "works for simple evaluation" $ do
    it "Constant" $ evalForTransaction (Constant 'a') dummyTransaction `shouldBe` 'a'
    it "Select (not found)" $ evalForTransaction (Select (Constant '?') (const Nothing) (Constant ())) dummyTransaction `shouldBe` '?'
    it "Select (found)" $ evalForTransaction (Select (Constant '?') (const $ Just '!') (Constant ())) dummyTransaction `shouldBe` '!'
    it "Account" $ evalForTransaction Account dummyTransaction `shouldBe` _account dummyTransaction
