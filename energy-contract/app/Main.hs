{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--- import Control.Applicative
import Control.Arrow ((&&&))
import Database.SQLite.Simple
--- import Database.SQLite.Simple.FromRow
import Data.List (sortOn)
import Data.Time.Calendar (addDays, fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone (..), utcToLocalTime)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

type Timestamp = Int

data Data202303Row = Data202303Row
  { row_timestamp :: Timestamp -- ^ seconds since unix epoch
  , row_pv2012_kWh :: Maybe Double -- ^ infrequent manual readings of PV installation's production (panels installed in 2012)
  , row_pv2022_kWh :: Maybe Double -- ^ automatic reading of second PV installation's production (panels installed in 2022)
  , row_peak_conso_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy consumption from the grid during peak hours
  , row_off_conso_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy consumption from the grid during off hours
  , row_peak_inj_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy injection into the grid during peak hours
  , row_off_inj_kWh :: Maybe Double -- ^ P1 meter data, cumulative energy injection into the grid during off hours
  , row_gas_m3 :: Maybe Double -- ^ infrequent manual readings of cumulative gas consumption
  , row_water_m3 :: Maybe Double -- ^ infrequent manual readings of cumulative water consumption
  } deriving (Show)

instance FromRow Data202303Row where
  fromRow = Data202303Row <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

interpolateLinear :: Timestamp -> Double -> Timestamp -> Double -> Timestamp -> Double
interpolateLinear unix_epoch_0 value_at_0 unix_epoch_1 value_at_1 unix_epoch
  | unix_epoch_0 == unix_epoch_1 = error "interpolateLinear: endpoint timestamps must differ"
  | otherwise = value_at_0 + (value_at_1 - value_at_0) * elapsed / interval
  where
    elapsed = fromIntegral $ unix_epoch - unix_epoch_0
    interval = fromIntegral $ unix_epoch_1 - unix_epoch_0

resampleSeries
  :: (a -> (Timestamp, Maybe Double))
  -> [a]
  -> (Timestamp -> Double -> Timestamp -> Double -> Timestamp -> Double)
  -> Timestamp
  -> Timestamp
  -> [(Timestamp, Double)]
resampleSeries extractor datarows interpolator unix_epoch_start unix_epoch_end
  | unix_epoch_start > unix_epoch_end = []
  | otherwise = go readings
  where
    readings =
      sortOn fst
        [ (timestamp, value)
        | (timestamp, Just value) <- map extractor datarows
        ]

    go [] = []
    go [(_, _)] = []
    go ((firstTimestamp, firstValue):rs@((_, _):_)) =
      let (beforeFirst, remaining) =
            span (<= firstTimestamp) sampleTimestamps
      in  [(t, firstValue) | t <- beforeFirst]
            ++ processIntervals (zip readings rs)
                                (snd $ last rs)
                                remaining

    sampleTimestamps =
      takeWhile (<= unix_epoch_end) $
        iterate (+ 15 * 60) unix_epoch_start

    processIntervals intervals lastValue =
      foldr processInterval (finish lastValue) intervals

    processInterval ((t0, v0), (t1, v1)) next timestamps =
      let (between, rest) = span (<= t1) timestamps
          values =
            [ (t, interpolator t0 v0 t1 v1 (fromIntegral t))
            | t <- between
            ]
      in values ++ next rest

    finish lastValue timestamps =
      [(t, lastValue) | t <- timestamps]

resamplePv2022Kwh :: [Data202303Row] -- ^ measurements
                  -> (Timestamp -> Double -> Timestamp -> Double -> Timestamp -> Double) -- ^ interpolation function, e.g. `interpolateLinear`
                  -> Timestamp -- ^ unix epoch of first timestamp in result
                  -> Timestamp -- ^ unix epoch of last timestamp in result
                  -> [(Timestamp, Double)] -- ^ resampled data
resamplePv2022Kwh = resampleSeries (row_timestamp &&& row_pv2022_kWh)

resampleConsoKwh :: [Data202303Row] -- ^ measurements
                 -> (Timestamp -> Double -> Timestamp -> Double -> Timestamp -> Double) -- ^ interpolation function, e.g. `interpolateLinear`
                 -> Timestamp -- ^ unix epoch of first timestamp in result
                 -> Timestamp -- ^ unix epoch of last timestamp in result
                 -> [(Timestamp, Double)] -- ^ resampled data
resampleConsoKwh = resampleSeries (row_timestamp &&& (\row -> (+) <$> row_peak_conso_kWh row <*> row_off_conso_kWh row))

resampleInjKwh :: [Data202303Row] -- ^ measurements
               -> (Timestamp -> Double -> Timestamp -> Double -> Timestamp -> Double) -- ^ interpolation function, e.g. `interpolateLinear`
               -> Timestamp -- ^ unix epoch of first timestamp in result
               -> Timestamp -- ^ unix epoch of last timestamp in result
               -> [(Timestamp, Double)] -- ^ resampled data
resampleInjKwh = resampleSeries (row_timestamp &&& (\row -> (+) <$> row_peak_inj_kWh row <*> row_off_inj_kWh row))

intervalDeltas :: [(Timestamp, Double)] -> [(Timestamp, Timestamp, Double)]
intervalDeltas readings =
  [ (start, end, endValue - startValue)
  | ((start, startValue), (end, endValue)) <- zip readings (drop 1 readings)
  ]

csvRows :: String -> String -> [(Timestamp, Double)] -> [(Timestamp, Double)] -> [String]
csvRows ean counter conso inj =
  case zipIntervals consoIntervals injIntervals of
    Nothing -> error "csvRows: consumption and production timestamps do not match"
    Just zipped ->
      concat
        [ [render "Consumption (+A) Totals" start end consoVol,
           render "Production (-A) Totals" start end injVol]
        | (start, end, consoVol, injVol) <- zipped
        ]
  where
    zipIntervals a b =
      foldr (\((st1, en1, vo1), (st2, en2, vo2)) tl ->
               if st1 == st2 && en1 == en2
               then fmap ((st1, en1, vo1, vo2):) tl
               else Nothing)
            (Just [])
          $ zip a b

    consoIntervals = intervalDeltas conso
    injIntervals = intervalDeltas inj

    render register start end volume =
      let (startDate, startTime) = formatLocalTimestamp start
          (endDate, endTime) = formatLocalTimestamp end
      in intercalateComma
           [startDate, startTime, endDate, endTime, ean, counter, register, printf "%f" volume]

    intercalateComma = foldr1 (\fld rest -> csvField fld <> "," <> rest) . map csvField

    csvField fld
      | any (`elem` (",\"\n\r" :: String)) fld =
          "\"" <> concatMap escapeQuote fld <> "\""
      | otherwise = fld

    escapeQuote '"' = "\"\""
    escapeQuote character = [character]

formatLocalTimestamp :: Timestamp -> (String, String)
formatLocalTimestamp timestamp =
  (formatTime defaultTimeLocale "%d-%m-%y" localTime,
   formatTime defaultTimeLocale "%H:%M:00" localTime)
  where
    utc = posixSecondsToUTCTime (fromIntegral timestamp)
    year = let (utcYear, _, _) = toGregorian (utctDay utc) in utcYear
    isSummerTime = summerTimeStart year <= utc && utc < summerTimeEnd year
    localTime = utcToLocalTime zone utc
    zone =
      TimeZone
        { timeZoneMinutes = if isSummerTime then 120 else 60
        , timeZoneSummerOnly = isSummerTime
        , timeZoneName = if isSummerTime then "CEST" else "CET"
        }

summerTimeStart :: Integer -> UTCTime
summerTimeStart year = transitionAtLastSunday year 3

summerTimeEnd :: Integer -> UTCTime
summerTimeEnd year = transitionAtLastSunday year 10

transitionAtLastSunday :: Integer -> Int -> UTCTime
transitionAtLastSunday year month =
  UTCTime lastSunday (secondsToDiffTime 3600)
  where
    nextMonth
      | month == 12 = fromGregorian (year + 1) 1 1
      | otherwise = fromGregorian year (month + 1) 1
    monthEnd = addDays (-1) nextMonth
    (_, _, weekday) = toWeekDate monthEnd
    lastSunday = addDays (negate (fromIntegral (weekday `mod` 7))) monthEnd

reconstructPv2012kWh
  :: [Data202303Row]
  -> [(Timestamp, Double)]
  -> [(Timestamp, Double)]
reconstructPv2012kWh datarows interpolatedPv2022 =
  case calibrationPoints of
    [] ->
      error "reconstructPv2012kWh: no pv2012_kWh calibration readings"
    [_] ->
      error "reconstructPv2012kWh: at least two pv2012_kWh readings are required"
    _ ->
      reconstructSorted calibrationPoints interpolatedPv2022
  where
    -- Both inputs are assumed sorted by timestamp.
    proxySeries = interpolatedPv2022

    candidateRows =
      [ (row_timestamp row, pv2012Value)
      | row <- datarows
      , Just pv2012Value <- [row_pv2012_kWh row]
      ]

    proxyValues =
      proxyValuesAt proxySeries (map fst candidateRows)

    calibrationPoints =
      [ (timestamp, proxyValue, pv2012Value)
      | ((timestamp, pv2012Value), (_, proxyValue)) <-
          zip candidateRows proxyValues
      ]

    -- Consume sorted queries as the sorted readings advance. Each query is
    -- consumed once; the foldr builds the interval processors in timestamp
    -- order.
    proxyValuesAt [] _ = []
    proxyValuesAt ((firstTimestamp, firstValue) : readings) queries =
      let (beforeFirst, remaining) = span (<= firstTimestamp) queries

          intervals =
            zip
              ((firstTimestamp, firstValue) : readings)
              readings

          processInterval ((t0, v0), (t1, v1)) next timestamps =
            let (here, rest) = span (<= t1) timestamps
                values =
                  [ (t, interpolateLinear t0 v0 t1 v1 t)
                  | t <- here
                  ]
            in values ++ next rest

          finish timestamps =
            [(t, lastValue) | t <- timestamps]

          lastValue = snd (last ((firstTimestamp, firstValue) : readings))
      in  [(t, firstValue) | t <- beforeFirst]
            ++ foldr processInterval finish intervals remaining

    reconstructSorted points queries =
      let intervals = zip points (drop 1 points)

          processInterval
            ((_, proxy0, value0), (t1, proxy1, value1))
            next
            timestamps =
              let (here, rest) = span ((<= t1) . fst) timestamps
                  values =
                    [ (timestamp, linearInterpolateForDouble proxy0 value0 proxy1 value1 proxy)
                    | (timestamp, proxy) <- here
                    ]
              in values ++ next rest

          (previous, lastPoint) = lastTwo points

          finish timestamps =
            [ (timestamp, linearInterpolateForDouble proxyPrevious valuePrevious proxyLast valueLast proxy)
            | (timestamp, proxy) <- timestamps
            ]
            where
              (_, proxyPrevious, valuePrevious) = previous
              (_, proxyLast, valueLast) = lastPoint

      in foldr processInterval finish intervals queries

    lastTwo = foldl
      (\(_, previous) current -> (previous, current))
      (error "lastTwo: expected at least two points", error "lastTwo: expected at least two points")

    -- not `interpolateLinear` because types don't match
    linearInterpolateForDouble x0 y0 x1 y1 x
      | x0 == x1 = error "reconstructPv2012kWh: calibration values must differ"
      | otherwise = y0 + (y1 - y0) * (x - x0) / (x1 - x0)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> test
    ["csv", start, end] -> runCsv start end "EAN" "COUNTER"
    ["csv", start, end, ean, counter] -> runCsv start end ean counter
    [] -> run
    _ -> fail "Usage: energy-contract [test | csv START_EPOCH END_EPOCH [EAN COUNTER]]\n\
              \       cabal run -- energy-contract csv $(date +%s --date=2025-03-15T00:00:00+01:00)\
                                                     \ $(date +%s --date=2026-03-15T11:15:00+01:00)\
                                                     \ \"'EAN'\" COUNTER"

run :: IO ()
run = do
  conn <- open "test.db"
  r <- query_ conn "SELECT * from data_202303 where pv2012_kWh is not null order by timestamp desc limit 10" :: IO [Data202303Row]
  mapM_ print r
  close conn

runCsv :: String -> String -> String -> String -> IO ()
runCsv startArg endArg ean counter =
  case (readMaybe startArg, readMaybe endArg) of
    (Just start, Just end)
      | start <= end -> do
          conn <- open "test.db"
          measurements <- query_ conn "SELECT * FROM data_202303 ORDER BY timestamp" :: IO [Data202303Row]
          close conn
          let conso = resampleConsoKwh measurements interpolateLinear start end
              inj = resampleInjKwh measurements interpolateLinear start end
              header = "Date de début,Heure de début,Date de fin,Heure de fin,EAN,Compteur,Registre,Volume"
          putStrLn header
          mapM_ putStrLn (csvRows ean counter conso inj)
      | otherwise -> fail "START_EPOCH must be less than or equal to END_EPOCH"
    _ -> fail "START_EPOCH and END_EPOCH must be Unix timestamps in seconds"

test :: IO ()
test =
  if interpolateLinear 0 0 10 30 5 == 15
      && resamplePv2022Kwh [] interpolateLinear 0 1800 == []
      && resamplePv2022Kwh [last measurements] interpolateLinear 0 1800 == []
      && resamplePv2022Kwh measurements interpolateLinear 0 1800 == [(0, 100), (900, 101), (1800, 102)]
      && resamplePv2022Kwh measurements interpolateLinear 0 2700 == [(0, 100), (900, 101), (1800, 102), (2700, 105)]
      && reconstructPv2012kWh calibrationMeasurements [(0, 0), (900, 100), (1800, 150), (2700, 250)]
          == [(0, 10), (900, 30), (1800, 40), (2700, 60)]
      && reconstructPv2012kWh calibrationMeasurements [(900, 100), (1800, 150)]
          == [(900, 10), (1800, 60)]
      && reconstructPv2012kWh calibrationMeasurements [(-900, -100), (0, 0), (900, 100), (1800, 150), (2700, 250)]
          == [(-900, -10), (0, 10), (900, 30), (1800, 40), (2700, 60)]
      && formatLocalTimestamp 1793452800 == ("31-10-26", "14:20:00") -- CET
      && formatLocalTimestamp 1792848000 == ("24-10-26", "15:20:00") -- CEST
    then putStrLn "tests passed"
    else fail "interpolation test failed"
  where
    measurements =
      [ Data202303Row 0 Nothing (Just 100) Nothing Nothing Nothing Nothing Nothing Nothing
      , Data202303Row 92 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      , Data202303Row 1800 Nothing (Just 102) Nothing Nothing Nothing Nothing Nothing Nothing
      , Data202303Row 1803 Nothing (Just 105) Nothing Nothing Nothing Nothing Nothing Nothing
      ]
    calibrationMeasurements =
      [ Data202303Row 0 (Just 10) (Just 0) Nothing Nothing Nothing Nothing Nothing Nothing
      , Data202303Row 900 Nothing (Just 100) Nothing Nothing Nothing Nothing Nothing Nothing
      , Data202303Row 1800 Nothing (Just 150) Nothing Nothing Nothing Nothing Nothing Nothing
      , Data202303Row 2700 (Just 60) (Just 250) Nothing Nothing Nothing Nothing Nothing Nothing
      ]
