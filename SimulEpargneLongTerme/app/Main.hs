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
    return $ (normal `mappend` long
             , filter (\Transaction { _account=a, _comment=Comment x _ } ->
                         (a == Normal) && (x == Deposit))
                      transactions
             , end)
  where loop start end sixtieth sixtyFirst = do
          if start > end
            then return ()
            else let depositDate = after start 1 1
                     taxDate = after depositDate 5 1
                     feeDate = after taxDate 12 31
                 in do
                      depositLongTerm depositDate
                      refundTax taxDate
                      if (sixtieth == taxDate) || (sixtieth < taxDate && taxDate < sixtyFirst)
                        then taxAt60
                        else return ()
                      deductFees feeDate
                      loop feeDate end sixtieth sixtyFirst

balanceAtRate :: Day -> Double -> [Transaction] -> Amount
balanceAtRate date rate = foldMap (\(Transaction { _amount=a, _date=d }) ->
                                     compoundAmount d rate date a)
  
main :: IO ()
main =
  let startDate = fromGregorian 2019 10 1 in
  flip mapM_ [1965, 1969, 1973, 1977, 1981, 1985] $ \y -> do
    putStrLn $ "\n----- Born " ++ show y ++ "-01-01, starting " ++ show startDate ++ " -----"
    let (finalTotal, s, end) = fst $ runSimulation (simulation startDate) y 1 1
    putStrLn $ "final total == " ++ showAmount finalTotal
    -- Show table of value of same deposits at various fixed interest rates:
    flip mapM_ [15, 20, 25, 30, 35 :: Int] $ \rate1000 ->
      putStrLn $ "fixed 0.0"
                 ++ show rate1000 ++ "%-> "
                 ++ (showAmount $ balanceAtRate end (fromIntegral rate1000 / 1000.0) s)
