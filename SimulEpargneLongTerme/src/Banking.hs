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
  , tabulateTransaction
  , scaleAmount
  , addTax
  , showAmount
  , compoundAmount
  -- actions
  , addYearlyInterest
  , balance
  , takeOutOldMoney
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
import Control.Monad (when)
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
showAmount (Amount a) = let eur = abs a `div` 100
                            cents = abs a `mod` 100
                            sign = if a < 0 then "-" else ""
                        in sign ++ show eur ++ case cents of
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

data XComment = Deposit | Withdrawal | TaxRefund | Fee | Interest | OldMoney
  deriving (Show, Eq)

data Comment = Comment XComment String
  deriving (Show, Eq)

data Transaction = Transaction {
  _account :: Account
  , _date :: Day
  , _amount :: Amount
  , _comment :: Comment
} deriving (Show, Eq)

tabulateTransaction :: Transaction -> String
tabulateTransaction (Transaction { _account=a, _date=d, _amount=m, _comment=Comment _ s }) =
    "| " ++ show d ++ " | "
         ++ showAccount a ++ " | "
         ++ showAmount' ++ " |  |  | "
         ++ (take (max 30 $ length s) $ s ++ repeat ' ') ++ " |"
  where showAccount Normal = "n"
        showAccount LongTerm = "L"
        showAmount' = let m' = showAmount m
                          p = "        "
                      in drop (length m') $ p ++ m'

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

funBalanceOlderThan :: Day -> Account -> [Transaction] -> Amount
funBalanceOlderThan date account transactions =
  let isRelevantTransaction (Transaction { _account=a, _date=d, _amount=(Amount m) }) =
        (a == account) -- only for account uner scrutiny,
        && ((d < date) -- all transactions that happened before
         || (m < 0))   -- any withdrawal: oldest money gets spent first
  in foldl' (\acc trnsctn -> acc <> _amount trnsctn)
            (Amount 0)
          $ filter isRelevantTransaction transactions

-- It is allowed to recover money from the long term account after 10
-- years without giving up the fiscal benefit: use that to top up fors
-- next year's deposit.  Do not take more than that: we assume that the
-- long term account still has a better interest rate than the normal
-- account.
takeOutOldMoney :: Day -> Simulation ()
takeOutOldMoney date = do
    blnc <- balance date Normal
    when (blnc < taxRefundableLongTermDeposit) $ do
      transactions <- lift $ gets _transactions
      let oldFunds = funBalanceOlderThan date10YearsAgo LongTerm transactions
      when (oldFunds > Amount 0) $ do
        let toTake = min oldFunds $ taxRefundableLongTermDeposit <> (scaleAmount (-1.0) blnc)
        makeTransaction date
                        LongTerm
                        (scaleAmount (-1.0) toTake)
                        Withdrawal
                      $ showAmount oldFunds ++ " older than " ++ show date10YearsAgo ++ " on " ++ show date
        makeTransaction date
                        Normal
                        toTake
                        OldMoney
                      $ "Top up with older than " ++ show date10YearsAgo ++ " on " ++ show date
  where (y, m, d) = toGregorian date
        date10YearsAgo = fromGregorian (y - 10) m d

topUp :: Day -> Amount -> Simulation ()
topUp date target = do
  blnc <- balance date Normal
  when (blnc < target) $ do
    makeTransaction date Normal (target <> scaleAmount (-1.0) blnc) Deposit $ "Top up to " ++ showAmount target

taxRefundableLongTermDeposit :: Amount
taxRefundableLongTermDeposit = Amount 226000

refundTax :: Day -> Simulation Bool
refundTax date =
  let (y, _, _) = toGregorian date
      -- There is a 2 year delay: money deposited in year n is declared
      -- during year n + 1 and the state gives back the money in year n + 2
      declarationYear = y - 2
      longTermDepositTwoYearsAgo (Transaction { _account=a, _date=d, _comment=Comment x _ }) =
        (a == Normal)
        && (x == Withdrawal)
        && (let (depositYear, _, _) = toGregorian d in depositYear == declarationYear)
  in do
    longDeposits <- fmap (filter longTermDepositTwoYearsAgo)
                       $ lift $ gets _transactions
    let amountSpentTwoYearsAgo = scaleAmount (-1.0) $ foldMap _amount longDeposits
    let gotRefund = amountSpentTwoYearsAgo > mempty
    when gotRefund $ do
      let refund = scaleAmount 0.32 $ min amountSpentTwoYearsAgo taxRefundableLongTermDeposit
      makeTransaction date Normal refund TaxRefund $ "Refund for " ++ showAmount amountSpentTwoYearsAgo ++ " in " ++ show declarationYear
    return gotRefund

taxAt60 :: Simulation ()
taxAt60 = do
  sixtieth <- asks _60th
  blnc <- balance sixtieth LongTerm
  makeTransaction sixtieth LongTerm (scaleAmount (-0.1) blnc) Withdrawal "Tax at 60"

depositLongTerm :: Day -> Simulation ()
depositLongTerm day = do
  entryFee <- asks _entryFee
  topUp day taxRefundableLongTermDeposit
  makeTransaction day Normal (scaleAmount (-1.0) taxRefundableLongTermDeposit) Withdrawal "Long term"
  makeTransaction day LongTerm (addTax (-1.0 * entryFee) taxRefundableLongTermDeposit) Deposit $ "Long term"

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
