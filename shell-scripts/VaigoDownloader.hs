#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (p: [p.turtle])"

{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.List as L
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Turtle


-- ./HaskellVaigoDownloader.hs --session 'ey...3D' --url 'https://vaigo.me/registrations/approved?budget=&budgetId=&budgetReasonId=&serviceKey=all&startDate=2023-03-01&endDate=2023-03-31'


data CliArgs = CliArgs
  { vaigoSession :: Text
  , startUrl :: Text
  } deriving (Show, Eq)


optionsParser :: Parser CliArgs
optionsParser = CliArgs <$> optText "session" 's' "Vaigo.me session cookie"
                        <*> optText "url" 'u' "Vaigo.me start URL"


main = sh $ do
  liftIO $ setLocaleEncoding utf8
  cliArgs <- options "Vaigo Downloader" optionsParser
  alreadySeen <- liftIO $ newIORef []
  let lines = crawlUrl (doCurl $ vaigoSession cliArgs) (startUrl cliArgs) alreadySeen
  Right expenses <- liftIO $ lines `fold` Fold parserStep initialState extractResult
  liftIO $ print expenses


doCurl sessionCookie url = inproc "curl" ["--silent", "--location", "--cookie", "vaigo_session=" <> sessionCookie, url] empty


crawlUrl :: (Text -> Shell Line) -> Text -> IORef [Text] -> Shell Line
crawlUrl getUrl url alreadySeenRef = do
    line <- T.strip . lineToText <$> getUrl url
    case (htmlLinkElementPattern `T.isInfixOf` line && matchNextRel line, "<span>" `T.isInfixOf` line) of
      (True, False) -> handleLink line
      (False, True) -> select $ textToLines line
      _ -> nop
  where htmlLinkElementPattern = "<a "
        matchNextRel = T.isInfixOf "rel=\"next\""
        nop = select []
        handleLink line =
          let relNextAhtmlElements = L.filter matchNextRel $ T.splitOn htmlLinkElementPattern line in
          case L.nub $ L.filter ("https://vaigo.me" `T.isPrefixOf`) $ concatMap (T.split (== '"')) relNextAhtmlElements of
            [] -> nop
            [nextUrl] -> do
              alreadySeen <- liftIO $ readIORef alreadySeenRef
              let decodedUrl = T.intercalate "&" $ T.splitOn "&amp;" nextUrl in
                if decodedUrl `elem` alreadySeen
                then do
                  -- liftIO $ putStrLn . T.unpack $ "Skipping " <> url <> ", already in " <> T.intercalate ", " alreadySeen
                  nop -- fail "Nothing more to do"
                else do
                  liftIO $ writeIORef alreadySeenRef $ decodedUrl:alreadySeen
                  -- liftIO $ putStrLn . T.unpack $ "recurse into " <> decodedUrl
                  crawlUrl getUrl decodedUrl alreadySeenRef
            moreUrls -> error $ "More than one next URL: " <> show moreUrls


data ExpenseType =
  CarRentalSelfService
  | FlexMobilityBudget
  | InternationalRailSelfService
  | SoftMobilitySelfService
  | TaxiSelfService
  | TripRegistration
  deriving (Eq, Show)

data Expense = Expense
  { expenseType :: ExpenseType
  , comment :: Text
  , ddmmyyyy :: (Int, Int, Int)
  , amountInCents :: Int
  } deriving (Eq, Show)

data ParserState = ParserState
  { psExpenseType :: Maybe ExpenseType
  , psComment :: Maybe Text
  , psDdMmYyyy :: Maybe (Int, Int, Int)
  , psExpenses :: [Expense]
  , psErrors :: [Text] -- unused at the moment
  } deriving (Eq, Show)


parserStep :: ParserState -> Line -> ParserState
parserStep state line = case unSpan $ lineToText line of
  Just txt -> _parserStep state txt
  Nothing -> state


initialState :: ParserState
initialState = ParserState {
  psExpenseType = Nothing
  , psComment = Nothing
  , psDdMmYyyy = Nothing
  , psExpenses = []
  , psErrors = []
  }

extractResult :: ParserState -> Either [Text] [Expense]
extractResult ParserState { psErrors = [], psExpenses } = Right psExpenses
extractResult ParserState { psErrors } = Left psErrors


_parserStep :: ParserState -> Text -> ParserState
_parserStep s@(ParserState { psExpenseType = Nothing }) "Car rental self-service" = s { psExpenseType = Just CarRentalSelfService, psComment = Nothing, psDdMmYyyy = Nothing }
_parserStep s@(ParserState { psExpenseType = Nothing }) "Flex Mobility budget" = s { psExpenseType = Just FlexMobilityBudget, psComment = Nothing, psDdMmYyyy = Nothing }
_parserStep s@(ParserState { psExpenseType = Nothing }) "International rail self-service" = s { psExpenseType = Just InternationalRailSelfService, psComment = Nothing, psDdMmYyyy = Nothing }
_parserStep s@(ParserState { psExpenseType = Nothing }) "Soft mobility self-service" = s { psExpenseType = Just SoftMobilitySelfService, psComment = Nothing, psDdMmYyyy = Nothing }
_parserStep s@(ParserState { psExpenseType = Nothing }) "Taxi self-service" = s { psExpenseType = Just TaxiSelfService, psComment = Nothing, psDdMmYyyy = Nothing }
_parserStep s@(ParserState { psExpenseType = Nothing }) "Trip registration" = s { psExpenseType = Just TripRegistration, psComment = Nothing, psDdMmYyyy = Nothing }
_parserStep s@(ParserState { psExpenseType = Nothing }) _ = s
_parserStep s@(ParserState { psExpenseType = Just _, psDdMmYyyy = Nothing }) txt = case (parseDdMmYyyy txt, psComment s) of
  (Just dmy@(dd, mm, yyyy), _) -> s { psDdMmYyyy = Just dmy }
  (Nothing, Just _) -> s
  (Nothing, Nothing) -> s { psComment = Just txt }
_parserStep s@(ParserState { psExpenseType = Just et, psDdMmYyyy = Just dmy, psExpenses = psExpenses }) txt = case (parseAmountInCents txt, psComment s) of
  (Just amountInCents, co) -> s {
    psExpenses = (Expense { expenseType = et, comment = fromMaybe "" co, ddmmyyyy = dmy, amountInCents = amountInCents}):psExpenses
    , psExpenseType = Nothing
    , psComment = Nothing
    , psDdMmYyyy = Nothing }
  (Nothing, Just _) -> s
  (Nothing, Nothing) -> s { psComment = Just txt }


parseAmountInCents :: Text -> Maybe Int
parseAmountInCents txt = do
  [_currencySymbol, amount] <- return $ T.split (== ' ') txt
  [eurosStrWithThousandsSep, centsStr] <- return $ T.split (== '.') amount
  let eurosStr = T.concat $ T.split (== ',') eurosStrWithThousandsSep
  euros <- readMaybe $ T.unpack eurosStr
  cents <- readMaybe $ T.unpack centsStr
  return $ 100 * euros + cents


parseDdMmYyyy :: Text -> Maybe (Int, Int, Int)
parseDdMmYyyy txt = case map (readMaybe . T.unpack) $ T.split (== '/') txt of
  [Just d, Just m, Just y] | 1 <= d && d <= 31 && 1 <= m && m <= 12 && 2022 <= y && y <= 2044 -> Just (d, m, y)
  _ -> Nothing


unSpan :: Text -> Maybe Text
unSpan txt = case (preSpan `T.isPrefixOf` txt, postSpan `T.isSuffixOf` txt) of
  (True, True) -> Just $ T.drop preSpanLen $ T.dropEnd postSpanLen txt
  _ -> Nothing
  where preSpan = "<span>"
        postSpan = "</span>"
        preSpanLen = T.length preSpan
        postSpanLen = T.length postSpan

-- Local Variables:
-- mode: haskell-mode
-- haskell-process-type: ghci
-- haskell-process-path-ghci: ("nixshellpp" "-p" "cabal-install" "-p" "haskellPackages.ghcWithPackages (p: [p.turtle p.haskell-language-server])" "++" "ghci")
-- End:
