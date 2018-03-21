module Main where

import Data.Monad.State

import Data.List (find)

import Data.Time.Calendar
  ( Day
  , addDays
  , addGregorianYearsClip
  , fromGregorian
  , toGregorian
  )

import Banking

data SavingsCombo = SavingsCombo {
  _normalAccount :: FixedInterestAccount
  , _longTerm :: FixedInterestAccount
  , _birthday :: Day -- contract terminates at a given age
  , _feeRate :: Double -- management fee for _longTerm
  , _feeCap :: Amount -- management fee is capped
  , _longTermTaxRate :: Double -- deposits are taxed
  , _now :: Day
  }
  deriving (Show, Eq)

advanceTime :: Int -> Int -> State SavingsCombo ()
advanceTime month day = modify updateDay
  where updateDay :: SavingsCombo -> SavingsCombo
        updateDay combo@(SavingsCombo { _now=now }) =
          combo { _now=after now month day }

topUpNormalAccount :: State SavingsCombo ()
topUpNormalAccount = do
  now <- gets _now
  
  
after :: Day -> Int -> Int -> Day
after t month day =
  let (y, _, _) = toGregorian t
      sameYear = fromGregorian y month day
      nextYear = fromGregorian (y + 1) month day
  in if sameYear <= t then nextYear else sameYear

doBeforeStopAfter :: Day -> (Day -> a -> a) -> Day -> a -> a
doBeforeStopAfter deadline f now x
  | now > deadline = x
  | otherwise = f now x

yearlyDeposit :: Double -> Amount -> Day -> Day -> FixedInterestAccount
yearlyDeposit rate amount = go (fiaNew rate)
  where go :: FixedInterestAccount -> Day -> Day -> FixedInterestAccount
        go account from to
          | from > to = account
          | otherwise = go (fiaDeposit account $ Transaction amount from $ "Deposit " ++ show amount)
                           (addGregorianYearsClip 1 from)
                           to

simul rate birth start end = go (fiaNew rate) (after start 1 1)
  where after t month day = let (y, m , r) = toGregorian t
                                d1 = fromGregorian y month day
                                d2 = fromGregorian (y + 1) month day
                            in if d1 > t then d1 else d2
        deductFee t (Amount x) =
          Transaction (Amount $ round $ fromInteger * 0.96)
                      t
                    $ Comment Deposit $ "Deposit " ++ show x ++ "-fees"
        go account t
           | t > end = account
           | otherwise = goTaxRefund (fiaDeposit account $ deductFee t $ Amount 213000)
                                   $ after t 5 1
        canDeduct targetYear (Transaction { _date=d, _comment=Comment typ _ }) =
          let (y, _, _) = fromGregorian d
          in y == targetYear && typ == Deposit
        taxRefund t (Transaction { _amount=Amount a }) =
          Transaction { _amount=Amount . round $ fromInteger a * 0.3
                        ,  _date=t
                        , _comment=Comment TaxRefund $ "Refund " ++ show a }
        goTaxRefund account@(FIA { _ledger=ledger }) t
           | t > end = account
           | otherwise = let (y, _, _) = fromGregorian t
                             mbTransaction = find (canDeduct $ y - 1) ledger
                         in go (maybe account
                                      (fiaDeposit account . taxRefund t)
                                      mbTransaction)
                               (after t 1 1)

main :: IO ()
main = putStrLn . show $ yearlyDeposit 0.01 (Amount 100) (fromGregorian 2012 1 1) (fromGregorian 2015 1 1)
