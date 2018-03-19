module Banking
  ( Amount(..)
  , FixedInterestAccount
  , Transaction(..)
  , Comment
  , _Comment(..)
  , compound
  , fiaBalance
  , fiaDeposit
  , fiaNew
  ) where

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Time.Calendar
  ( Day
  , diffDays
  , fromGregorian
  , toGregorian
  )

newtype Amount = Amount Int
  deriving (Show, Eq, Ord)

instance Monoid Amount where
  mempty = Amount 0
  mappend (Amount a) (Amount b) = Amount $ a + b

compound :: Amount -> Double -> Day -> Day -> Amount
compound amount@(Amount 0) _ _ _ = amount
compound amount 0.0 _ _ = amount
compound (Amount money) yearlyRate from to =
  let diff = to `diffDays` from
      mult = (1.0 + yearlyRate) ** (fromInteger diff / 365.0)
  in Amount $ round $ fromIntegral money * mult

data _Comment = Deposit | ManagementFee | TaxRefund
  deriving (Show, Eq)

data Comment = Comment _Comment String
  deriving (Show, Eq)

data Transaction = Transaction { _amount :: Amount, _date :: Day, _comment :: Comment }
  deriving (Show)

data FixedInterestAccount = FIA {
  _ledger :: [Transaction]
  , _rate :: Double
  }
  deriving (Show)

fiaNew r = FIA { _ledger=[], _rate=r }

fiaBalance (FIA { _ledger=ledger, _rate=r }) date =
  let updateBalance :: Amount -> Transaction -> Amount
      updateBalance prevAmount (Transaction { _amount=a, _date=d })
        | d > date = prevAmount
        | otherwise = compound a r d date <> prevAmount
  in foldl' updateBalance mempty ledger

fiaDeposit fia@(FIA { _ledger=ledger }) transaction =
  fia { _ledger=transaction:ledger }

-- Local Variables:
-- intero-targets: "SimulEpargneLongTerme:library"
-- End:
