module Main where

import Data.Function (on)
import Data.List (sortBy)
import Data.Time.Calendar
  ( Day
  , addGregorianYearsClip
  , fromGregorian
  , toGregorian
  )
import Control.Monad (when)
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

-- | Run a simulation from the first long term deposit until the long
--   term contract ends and all tax breaks are recovered.
simulation :: Day -- ^ date of 1st long term deposit, all other deposits happen on Jan, 1st in next years
           -> Simulation (Amount, [Transaction], Day) -- ^ (total money in normal + long term account
                                                      --   , list of transactions for simulation
                                                      --   , date of last transaction in simulation)
simulation simulationStart = do
    -- that's the end of the contract, but simulation will need to run
    -- longer to account for delay in recovering tax break:
    end <- asks _end
    sixtieth <- asks _60th
    -- first year is special: long term deposit happens on
    -- simulationStart which isn't necessarily Jan 1st.
    depositLongTerm simulationStart
    deductFees $ after simulationStart 12 31
    let (firstYear, _, _) = toGregorian simulationStart
    addYearlyInterest firstYear LongTerm
    addYearlyInterest firstYear Normal
    -- `loop' will first advance to Jan 1 after simulationStart
    loop simulationStart end sixtieth $ addGregorianYearsClip 1 sixtieth
    transactions <- lift $ gets _transactions
    let simulationEnd = maximum $ map _date transactions
    normal <- balance simulationEnd Normal
    long <- balance simulationEnd LongTerm
    return $ (normal `mappend` long
             -- normally, reversing the transaction list should be
             -- sufficient, sorting the transactions just to be on the
             -- safe side & explicit about the goal:
             , sortBy (compare `on` _date) $ reverse transactions
             , simulationEnd)
  where loop start end sixtieth sixtyFirst =
          let [_, depositDate, taxDate, takeOutDate, feeDate] =
                scanl (\p (m, d) -> after p m d)
                      start
                      [(1, 1), (5, 1),  (12, 1),     (12, 31)]
              (year, _, _) = toGregorian feeDate
              depositThisYear = start <= end
              depositNextYear = depositThisYear && addGregorianYearsClip 1 start <= end
          in do
               when depositThisYear $
                 depositLongTerm depositDate
               gotRefund <- refundTax taxDate
               when (sixtieth <= taxDate && taxDate < sixtyFirst)
                 taxAt60
               when (depositThisYear || gotRefund) $ do
                 when depositNextYear $
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
    flip mapM_ [29..32 :: Int] $ \rate1000 ->
      putStrLn $ "fixed "
                 ++ (show $ rate1000 `div` 10) ++ "." ++ (show $ rate1000 `mod` 10) ++ "%-> "
                 ++ (showAmount $ normalDepositsAtRate end (fromIntegral rate1000 / 1000.0) allTransactions)
    if y == 1977
    then mapM_ (putStrLn . tabulateTransaction) allTransactions
    else return ()
