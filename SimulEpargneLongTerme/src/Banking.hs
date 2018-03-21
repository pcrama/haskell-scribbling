module Banking
  ( Amount(..)
  , FixedInterestAccount
  , Transaction(..)
  , Comment
  , XComment(..)
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

scaleAmount :: Double -> Amount -> Amount
scaleAmount scale (Amount amount) = Amount $ round $ fromIntegral amount * scale

addTax :: Double -> Amount -> Amount
addTax = scaleAmount . (1.0 +)

compound :: Amount -> Double -> Day -> Day -> Amount
compound amount@(Amount 0) _ _ _ = amount
compound amount 0.0 _ _ = amount
compound amount yearlyRate from to =
  let diff = to `diffDays` from
      mult = (1.0 + yearlyRate) ** (fromInteger diff / 365.25)
  in scaleAmount mult amount

data XComment = Deposit | ManagementFee | TaxRefund | Withdrawal
  deriving (Show, Eq)

data Comment = Comment XComment String
  deriving (Show, Eq)

data Transaction = Transaction { _amount :: Amount, _date :: Day, _comment :: Comment }
  deriving (Show)

data FixedInterestAccount = FIA {
  _ledger :: [Transaction]
  , _rate :: Double
  }
  deriving (Show)

fiaNew :: Double -> FixedInterestAccount
fiaNew r = FIA { _ledger=[], _rate=r }

fiaBalance date (FIA { _ledger=ledger, _rate=r }) =
  let updateBalance :: Amount -> Transaction -> Amount
      updateBalance prevAmount (Transaction { _amount=a, _date=d })
        | d > date = prevAmount
        | otherwise = compound a r d date <> prevAmount
  in foldl' updateBalance mempty ledger

fiaAddTransaction fia@(FIA { _ledger=ledger }) transaction =
  fia { _ledger=transaction:ledger }

fiaDeposit t fia amount s =
  fiaAddTransaction fia $ Transaction { _amount=amount, _date=t, _comment = Comment Deposit s }

fiaTopUp t fia (Amount target) =
  let Amount balance = fiaBalance t fia
  in if balance > target
     then fia
     else fiaDeposit t fia (Amount $ target - balance) $ "Top up to " ++ (show $ fromIntegral target / 100.0)

fiaTaxRefund t fia amountSpentLastYear =
  let (y, _, _) = toGregorian t
  in fiaAddTransaction fia $ Transaction { _amount=refund
                                         , _date=t
                                         , _comment=Comment TaxRefund
                                                          $ "Tax refund for " ++ (show $ fromIntegral amountSpentLastYear / 100.0) ++ " saved in " ++ (show $ y - 1) }

fiaWithdraw t fia (Amount amount) xcomment s =
  fiaAddTransaction fia $ Transaction { _amount=Amount $ 0 - (abs amount)
                                      , _date=t
                                      , _comment=Comment xcomment s }

-- Local Variables:
-- intero-targets: "SimulEpargneLongTerme:library"
-- End:
