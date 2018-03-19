module Main where

import Data.List (find)

import Data.Time.Calendar
  ( Day
  , addDays
  , addGregorianYearsClip
  , fromGregorian
  , toGregorian
  )

import Banking

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
        canDeduct targetYear (Transaction { _date=d, _comment=Comment typ _ })
          let (y, _, _) = fromGregorian d
          in y == targetYear && typ == Deposit
        taxRefund t (Transaction { _amount=Amount a }) =
          Transaction { _amount=Amount . round $ fromInteger a * 0.3,
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
