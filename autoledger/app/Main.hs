{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.ByteString as BL
import Data.Bifunctor (first, bimap)
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import qualified Data.Text.IO as TIO
import Data.Time (toGregorian)
import System.Environment (getArgs)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Lib
  ( Classifiers (..),
    ITransaction (..),
    LedgerEntry (..),
    UnstructuredData (..),
    columnsToBelfius,
    compileConfigFile,
    mkLedgerEntry,
    packShow,
    packShow0Pad,
    parseConfigFileText,
    runUnstructuredDataParser,
    squeeze,
  )

newline :: T.Text
newline = "\n"

prefixForRowAsComment :: T.Text
prefixForRowAsComment = ";<-"

copyRowAsComment :: T.Text -> T.Text
copyRowAsComment x = prefixForRowAsComment <> x

renderAccountUpdate :: Lib.LedgerEntry -> [T.Text]
renderAccountUpdate ledgerEntry =
  let account = Lib.ledgerAccount ledgerEntry
      amountCents = Lib.ledgerAmountCents ledgerEntry
      currency = Lib.ledgerCurrency ledgerEntry
      prefix = "    "
      target = 52
      left = prefix <> account
      (units, cents) = abs amountCents `divMod` 100
      sign = if amountCents < 0 then "-" else mempty
      renderedAmount = sign <> Lib.packShow units <> "." <> Lib.packShow0Pad 2 cents
      extraSpaces = max 2 $ target - T.length left - T.length renderedAmount
      currencySymbol = case currency of
        "EUR" -> "€"
        _ -> currency
      secondLine = case Lib.ledgerOtherAccount ledgerEntry of
        Nothing -> []
        Just x
          | T.all isSpace x -> []
          | otherwise -> [prefix <> x]
   in (left <> T.replicate extraSpaces " " <> renderedAmount <> " " <> currencySymbol) : secondLine

renderDay :: Lib.LedgerEntry -> T.Text
renderDay t = Lib.packShow year <> "/" <> Lib.packShow0Pad 2 month <> "/" <> Lib.packShow0Pad 2 date
  where
    (year, month, date) = toGregorian $ Lib.ledgerDate t

renderDescription :: Lib.LedgerEntry -> T.Text
renderDescription = Lib.squeeze . Lib.ledgerText

renderTransaction ::
  (Show a, Lib.ITransaction r) =>
  Lib.Classifiers ->
  (Int, [T.Text]) ->
  Either a r ->
  [T.Text]
renderTransaction _ (lineNo, dataRow) (Left err) =
  [ copyRowAsComment $ T.intercalate ";" dataRow,
    "; error in line " <> T.pack (show lineNo)
  ]
  ++ map (T.pack . ("; " <>)) (lines $ show err)
  ++ [""]
renderTransaction c _ (Right transaction) =
  renderLedgerEntry $
    Lib.mkLedgerEntry
      (Lib.getLedgerTextClassifier c)
      (Lib.getAssetClassifier c)
      (Lib.getLedgerOtherAssetClassifier c)
      transaction

renderLedgerEntry :: Lib.LedgerEntry -> [T.Text]
renderLedgerEntry le =
  [ copyRowAsComment $ Lib.precedingComment le,
    renderDay le <> " " <> renderDescription le
  ]
  ++ renderAccountUpdate le
  -- Add trailing newline with [""]
  ++ [""]

data AppArgs = AppArgs {
  classifiersFile :: String
  , inputFile :: String
  , pastTransactionsFile :: Maybe String
  }

defaultAppArgs :: AppArgs
defaultAppArgs = AppArgs {
  classifiersFile = "app-config.lisp"
  , inputFile = "script-input.txt"
  , pastTransactionsFile = Nothing
  }

parseArgs :: [String] -> Either String AppArgs
parseArgs topXs@(('-':_):_) = go defaultAppArgs topXs
  where go a [] = Right a
        go a ("-s":f:xs) = go (a { classifiersFile = f }) xs
        go a ("--script":f:xs) = go (a { classifiersFile = f }) xs
        go a ("-i":f:xs) = go (a { inputFile = f }) xs
        go a ("--input":f:xs) = go (a { inputFile = f }) xs
        go a ("-p":f:xs) = go (a { pastTransactionsFile = Just f }) xs
        go a ("--past":f:xs) = go (a { pastTransactionsFile = Just f }) xs
        go _ (e:_) = Left $ "Unkown command line arg: '" <> e <> "'"
parseArgs [c, i] = pure AppArgs { classifiersFile = c, inputFile = i, pastTransactionsFile = Nothing }
parseArgs [] = pure defaultAppArgs
parseArgs xs = Left $ "Can't parse command line args" <> foldMap (" " <>) xs

extractPastRows :: [T.Text] -> Set T.Text
extractPastRows = Set.fromList
                . filter (prefixForRowAsComment `T.isPrefixOf`)

doMain :: AppArgs -> IO ()
doMain AppArgs { classifiersFile = clF , inputFile = inF , pastTransactionsFile = mbPastF } = do
  script <- readAndDecode clF
  bankData <- readAndDecode inF
  pastData <- case mbPastF of
                Just pastF -> (extractPastRows . map T.strip . T.lines) <$> readAndDecode pastF
                Nothing -> return Set.empty
  case parseScript script >>= compileScript >>= parseBankData bankData of
    Left e -> e
    Right (classifiers, ud) ->
      case Lib.udData ud of
        [] -> print $ Lib.udColumnNames ud
        d -> mapM_ TIO.putStrLn
                 $ concat
                 $ filter (isNotIn pastData)
                 $ map (uncurry (renderTransaction classifiers))
                       (reverse $ zip d $ Lib.columnsToBelfius ud)
  where readAndDecode f = do
          bytes <- BL.readFile f
          let (encoding, fileData) = case decodeUtf8' bytes of
                Left _ -> ("latin1", decodeLatin1 bytes)
                Right d -> ("utf8", d)
          putStrLn $ f <> " uses " <> encoding <> "."
          pure fileData
        parseScript = first print . Lib.parseConfigFileText clF
        compileScript = first print . Lib.compileConfigFile (clF, 0, 0)
        parseBankData b c = bimap print (c,) $ Lib.runUnstructuredDataParser inF b
        isNotIn p (firstLine:_) = not $ firstLine `Set.member` p
        isNotIn _ [] = True

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case parseArgs args of
    Left errorMessage -> foldMap putStrLn [
      errorMessage
      , "cabal new-run exe:autoledger -- [(-s | --script) app-config.lisp]\
                                       \ [(-i | --input) script-input.txt]\
                                       \ [(-p | --past) ledger.ledger]"]
    Right ci -> doMain ci

-- Local Variables:
-- compile-command: "(cd autoledger; cabal new-run exe:autoledger)"
-- coding: utf-8
-- End:
