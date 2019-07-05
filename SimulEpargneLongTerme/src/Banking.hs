module Banking (
  ContractInfo(..)
  , ContractState(..)
  , runSimulation
  , Amount(..)
  , Simulation
  , Account(..)
  , XComment(..)
  , Transaction(..)
  , Comment(..)
  , makeTransaction
  , scaleAmount
  , addTax
  , showAmount
  , compoundAmount
  -- actions
  , addYearlyInterest
  , balance
  , topUp
  , refundTax
  , depositLongTerm
  , taxAt60
  , deductFees
  ) 
where

import Data.Time.Calendar
import Data.List (foldl')
import Data.Monoid ((<>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

data ContractInfo = ContractInfo {
  _normalRate :: Double -- interest rate on `normal' account
  , _longRate :: Double -- interest rate on long term contract
  , _entryFee :: Double -- ratio of fees deduced when putting money in long term contract
  , _60th :: Day -- when the contract owner turns 60 some taxation is levied
  , _yearlyMgmtFee :: Double -- management fee for the long term contract
  , _yearlyMgmtCap :: Amount -- management fees are capped
  , _end :: Day
}

argentaContract :: ContractInfo
argentaContract = ContractInfo {
  _normalRate=0.005
  , _longRate=0.0105
  , _entryFee=0.06 -- 2% tax and 4% for Argenta
  , _60th=undefined
  , _yearlyMgmtFee=0.0027
  , _yearlyMgmtCap=Amount $ round (100.0 * 7500.0 * 0.0027 :: Double)
  , _end=undefined
}

mkContractInfo :: Integer -> Int -> Int -> ContractInfo
mkContractInfo y m d = argentaContract {
  _60th=fromGregorian (y + 60) m d
  , _end=fromGregorian (y + 65) m d
  }

newtype Amount = Amount Int
  deriving (Show, Eq, Ord)

showAmount :: Amount -> String
showAmount (Amount a) = let eur = a `div` 100
                            cents = a `mod` 100
                        in show eur ++ case cents of
                                        x | x == 0 -> ""
                                          | x < 10 -> ".0" ++ show x
                                          | otherwise -> "." ++ show x

instance Semigroup Amount where
  (Amount a) <> (Amount b) = Amount $ a + b

instance Monoid Amount where
  mempty = Amount 0
  -- mappend (Amount a) (Amount b) = Amount $ a + b

data Account = Normal | LongTerm
  deriving (Show, Eq)

data XComment = Deposit | Withdrawal | TaxRefund | Fee | Interest
  deriving (Show, Eq)

data Comment = Comment XComment String
  deriving (Show, Eq)

data Transaction = Transaction {
  _account :: Account
  , _date :: Day
  , _amount :: Amount
  , _comment :: Comment
} deriving (Show, Eq)

makeTransaction :: Day -> Account -> Amount -> XComment -> String -> Simulation ()
makeTransaction date account amount xcomment s = 
  lift
  $ modify (ContractState
          . (Transaction { _account=account
                         , _date=date
                         , _amount=amount
                         , _comment=Comment xcomment s
                         }:)
          . _transactions)

scaleAmount :: Double -> Amount -> Amount
scaleAmount x (Amount a) = Amount $ round $ x * fromIntegral a

addTax :: Double -> Amount -> Amount
addTax tax = scaleAmount $ 1 + tax

compoundAmount :: Day -> Double -> Day -> Amount -> Amount
compoundAmount t0 rate t1=
  scaleAmount $ (1 + rate) ** ((fromIntegral $ t1 `diffDays` t0) / 365.25)

newtype ContractState = ContractState { _transactions :: [Transaction] }
  deriving Show

type Simulation f = ReaderT ContractInfo (State ContractState) f

balance :: Day -> Account -> Simulation Amount
balance date account =
  let isRelevantTransaction (Transaction { _account=a, _date=d }) =
        (a == account) && (d <= date)
  in do
       transactions <- lift $ gets _transactions
       return $ foldl' (\acc trnsctn -> acc <> _amount trnsctn)
                       (Amount 0)
                     $ filter isRelevantTransaction transactions

topUp :: Day -> Amount -> Simulation ()
topUp date target = do
  blnc <- balance date Normal
  if blnc < target
  then makeTransaction date Normal (target <> scaleAmount (-1.0) blnc) Deposit $ "Top up to " ++ showAmount target
  else return ()

taxRefundableLongTermDeposit :: Amount
taxRefundableLongTermDeposit = Amount 226000

refundTax :: Day -> Simulation ()
refundTax date =
  let (y, _, _) = toGregorian date
      longTermDepositInPreviousYear (Transaction { _account=a, _date=d, _comment=Comment x _ }) =
        (a == LongTerm)
        && (x == Deposit)
        && (let (depositYear, _, _) = toGregorian d in depositYear + 1 == y)
  in do
    longDeposits <- fmap (filter longTermDepositInPreviousYear)
                       $ lift $ gets _transactions
    let amountSpentLastYear = foldMap _amount longDeposits
    if amountSpentLastYear > mempty
      then let refund = scaleAmount 0.32 $ min amountSpentLastYear taxRefundableLongTermDeposit
           in makeTransaction date Normal refund TaxRefund $ "Refund for " ++ showAmount amountSpentLastYear ++ " in " ++ show (y - 1)
      else return ()

taxAt60 :: Simulation ()
taxAt60 = do
  sixtieth <- asks _60th
  blnc <- balance sixtieth LongTerm
  makeTransaction sixtieth LongTerm (scaleAmount (-0.1) blnc) Withdrawal "Tax at 60"

depositLongTerm :: Day -> Simulation ()
depositLongTerm day = do
  amount <- fmap (\rate -> addTax rate taxRefundableLongTermDeposit)
          $ asks _entryFee
  topUp day amount
  makeTransaction day Normal (scaleAmount (-1.0) amount) Withdrawal "Long term + fees"
  makeTransaction day LongTerm taxRefundableLongTermDeposit Deposit $ show day

deductFees :: Day -> Simulation ()
deductFees day = do
  feeCap <- asks _yearlyMgmtCap
  feeRate <- asks _yearlyMgmtFee
  blnc <- balance day LongTerm
  makeTransaction day
                  LongTerm
                  (scaleAmount (-1.0) $ min feeCap $ scaleAmount feeRate blnc)
                  Fee
                $ "Fee for " ++ showAmount blnc

addYearlyInterest :: Integer -> Account -> Simulation ()
addYearlyInterest year account = do
    startBalance <- balance lastYearEnd account
    rate <- asks (case account of
                    Normal -> _normalRate
                    LongTerm -> _longRate)
    let interest (Transaction { _amount=a, _date=d }) =
          compoundAmount d rate yearEnd a <> scaleAmount (-1.0) a
    transactions <- lift $ gets _transactions
    let interestOfTransactionsInThisYear =
          foldl' (\acc trnsctn -> acc <> interest trnsctn)
                 (Amount 0)
               $ filter isRelevantTransaction transactions
    makeTransaction yearEnd
                    account
                    (scaleAmount rate startBalance <> interestOfTransactionsInThisYear)
                    Interest
                  $ "Interest for " ++ show year
  where [lastYearEnd, yearEnd] = map (\y -> fromGregorian y 12 31) [year - 1, year]
        isRelevantTransaction (Transaction { _account=a, _date=d }) =
          (a == account) && (lastYearEnd < d) && (d <= yearEnd)

-- | Run simulation for a person
runSimulation :: Simulation f -- ^ Simulation to run
              -> Integer -- ^ Birthday (year)
              -> Int -- ^ Birthday (month)
              -> Int -- ^ Birthday (day)
              -> (f, ContractState) -- ^ simulation result and final contract state
runSimulation s y m d = runState (runReaderT s $ mkContractInfo y m d) $ ContractState []

-- Local Variables:
-- intero-targets: "SimulEpargneLongTerme:library"
-- End:
