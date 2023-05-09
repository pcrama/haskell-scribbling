{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Home
    ( home
    , getReadings
    , postReading
    ) where

import           Data.Aeson (toJSON)
import           Text.Read (readMaybe)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time (DayOfMonth, MonthOfYear, Year, getCurrentTime, fromGregorian, gregorianMonthLength, toGregorian, utctDay)
import           Lib.SimpleReading (loadReadings, ReadingMeter, SimpleReading(..), readingMeter)
import           Views.Home (homeView, newReadingInputFormView, readingsView)
import           Web.Scotty (ActionM, ScottyM, get, param, post, raiseStatus, redirect)
import           Network.HTTP.Types.Status
import           Control.Monad.IO.Class (liftIO)


home :: ScottyM ()
home = get "/" homeView


data ParsedReadingMeter =
  NoReadingMeter
  | ReadingMeterParsingError Text
  | ParsedReadingMeter ReadingMeter


raiseMeterUnknownParseError :: Text -> ActionM a
raiseMeterUnknownParseError s = raiseStatus status404 $ "No meter '" <> s <> "' known."


getParsedReadingMeter :: Text -> ActionM ParsedReadingMeter
getParsedReadingMeter paramName = do
  meterStr :: String <- param paramName
  return $ case (meterStr, readMaybe meterStr :: Maybe ReadingMeter) of
    ("", _) -> NoReadingMeter
    (_, Just meterId) -> ParsedReadingMeter meterId
    (_, Nothing) -> ReadingMeterParsingError $ T.pack meterStr


getReadings :: ScottyM ()
getReadings = get "/readings/:meterId" $ do
  meterId <- getParsedReadingMeter "meterId"
  allMeasurements <- loadReadings
  case meterId of
    NoReadingMeter -> readingsView allMeasurements
    ParsedReadingMeter prm -> readingsView $ filter ((== prm) . readingMeter) allMeasurements
    ReadingMeterParsingError "new" -> do
      (year, month, day) <- toGregorian . utctDay <$> liftIO getCurrentTime
      newReadingInputFormView year month day
    ReadingMeterParsingError s -> raiseMeterUnknownParseError s


postReading :: ScottyM ()
postReading = post "/readings/" $ do
  parsedReadingMeter <- getParsedReadingMeter "meter"
  meter <- case parsedReadingMeter of
    ParsedReadingMeter prm -> return prm
    NoReadingMeter -> raiseStatus status400 "Meter ID is mandatory"
    ReadingMeterParsingError s -> raiseMeterUnknownParseError s
  year :: Year <- param "year"
  raiseWhenOutOfBounds 2003 year 2078 "year"
  month :: MonthOfYear <- param "month"
  raiseWhenOutOfBounds 1 month 12 "month"
  day :: DayOfMonth <- param "day"
  raiseWhenOutOfBounds 1 day (gregorianMonthLength year month) "day"
  value :: Float <- param "value"
  -- TODO: write to DB
  liftIO $ putStrLn . show . toJSON $ SimpleReading { readingTime=fromGregorian (fromIntegral year) month day, readingMeter=meter, readingValue=value }
  redirect $ "/readings/" <> (T.pack $ show meter)
  where raiseWhenOutOfBounds lower v upper name
          | lower <= v && v <= upper = return ()
          | otherwise = raiseStatus status400 $ name <> "=" <> (T.pack $ show v) <> " is invalid"
