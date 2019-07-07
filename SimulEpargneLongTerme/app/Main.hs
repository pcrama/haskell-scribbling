module Main where

import Data.Time.Calendar
  ( Day
  , addGregorianYearsClip
  , fromGregorian
  , toGregorian
  )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans.State (gets)
import Banking

after :: Day -> Int -> Int -> Day
after t month day =
  let (y, _, _) = toGregorian t
      sameYear = fromGregorian y month day
      nextYear = fromGregorian (y + 1) month day
  in if sameYear <= t then nextYear else sameYear

simulation :: Day -> Simulation (Amount, [Transaction], Day)
simulation simulationStart = do
    end <- asks _end
    sixtieth <- asks _60th
    loop simulationStart end sixtieth $ addGregorianYearsClip 1 sixtieth
    normal <- balance end Normal
    long <- balance end LongTerm
    transactions <- lift $ gets _transactions
    return $ (normal `mappend` long, reverse transactions, end)
  where loop start end sixtieth sixtyFirst = do
          if start > end
            then return ()
            else let [_, depositDate, taxDate, takeOutDate, feeDate] =
                       scanl (\p (m, d) -> after p m d)
                             start
                             [(1, 1), (5, 1),  (12, 1),     (12, 31)]
                     (year, _, _) = toGregorian feeDate
                 in do
                      depositLongTerm depositDate
                      refundTax taxDate
                      if (sixtieth == taxDate) || (sixtieth < taxDate && taxDate < sixtyFirst)
                        then taxAt60
                        else return ()
                      takeOutOldMoney takeOutDate
                      deductFees feeDate
                      addYearlyInterest year LongTerm
                      addYearlyInterest year Normal
                      loop feeDate end sixtieth sixtyFirst

normalDepositsAtRate :: Day -> Double -> [Transaction] -> Amount
normalDepositsAtRate date rate = foldMap valuateDeposit
  where valuateDeposit (Transaction { _amount=a
                                    , _date=d
                                    , _account=Normal
                                    , _comment=Comment Deposit _}) =
                             compoundAmount d rate date a
        valuateDeposit _ = mempty -- only count deposits on Normal account

main :: IO ()
main =
  let startDate = fromGregorian 2019 10 1 in
  flip mapM_ [1967, 1973, 1977, 1983, 1987] $ \y -> do
    putStrLn $ "\n----- Born " ++ show y ++ "-01-01, starting " ++ show startDate ++ " -----"
    let (finalTotal, allTransactions, end) = fst $ runSimulation (simulation startDate) y 1 1
    putStrLn $ "final total == " ++ showAmount finalTotal ++ " on " ++ show end
    -- Show table of value of same deposits at various fixed interest rates:
    flip mapM_ [22..25 :: Int] $ \rate1000 ->
      putStrLn $ "fixed "
                 ++ (show $ rate1000 `div` 10) ++ "." ++ (show $ rate1000 `mod` 10) ++ "%-> "
                 ++ (showAmount $ normalDepositsAtRate end (fromIntegral rate1000 / 1000.0) allTransactions)
    if y == 1977
    then mapM_ (putStrLn . tabulateTransaction) allTransactions
    else return ()
