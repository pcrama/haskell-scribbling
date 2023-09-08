{-# LANGUAGE TupleSections #-}
module Main where

import Control.Lens (toListOf, traversed)
import qualified Data.ByteString as BL
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import qualified Data.Text.IO as TIO
import Data.Time (toGregorian)
import System.Environment (getArgs)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Codec.Xlsx.Parser.Stream as XL
import qualified Lib
  ( Classifiers (..),
    ITransaction (..),
    IUnstructuredData (..),
    LedgerEntry (..),
    argentaReference,
    columnsToArgenta,
    columnsToBelfius,
    compileConfigFile,
    mkLedgerEntry,
    packShow,
    packShow0Pad,
    parseConfigFileText,
    parseXlsxRows,
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
  , inputFiles :: NonEmpty String
  , pastTransactionsFile :: Maybe String
  }

defaultAppArgs :: AppArgs
defaultAppArgs = AppArgs {
  classifiersFile = "app-config.lisp"
  , inputFiles = "script-input.txt" :| []
  , pastTransactionsFile = Nothing
  }

parseArgs :: [String] -> Either String AppArgs
parseArgs topXs@(('-':_):_) = go defaultAppArgs topXs
  where go a [] = Right a
        go a ("-s":f:xs) = go (a { classifiersFile = f }) xs
        go a ("--script":f:xs) = go (a { classifiersFile = f }) xs
        go a ("-i":xs) = gatherFiles a xs
        go a ("--input":xs) = gatherFiles a xs
        go a ("-p":f:xs) = go (a { pastTransactionsFile = Just f }) xs
        go a ("--past":f:xs) = go (a { pastTransactionsFile = Just f }) xs
        go _ (e:_) = Left $ "Unkown command line arg: '" <> e <> "'"
        gatherFiles a (x:xs) = Right $ a { inputFiles= x:|xs }
        gatherFiles _ [] = Left "No input files"
parseArgs [c, i] = pure AppArgs { classifiersFile = c, inputFiles = i:|[], pastTransactionsFile = Nothing }
parseArgs [] = pure defaultAppArgs
parseArgs xs = Left $ "Can't parse command line args" <> foldMap (" " <>) xs

extractPastRows :: [T.Text] -> Set T.Text
extractPastRows = Set.fromList
                . filter (prefixForRowAsComment `T.isPrefixOf`)

doMain :: AppArgs -> IO ()
doMain AppArgs { classifiersFile = clF , inputFiles = inFiles , pastTransactionsFile = mbPastF } = do
  script <- readAndDecodeText clF
  pastData <- case mbPastF of
                Just pastF -> (map T.strip . T.lines) <$> readAndDecodeText pastF
                Nothing -> return []
  case parseScript script >>= compileScript of
    Left e -> e
    Right classifiers -> do
      results <- traverse (parseBankData pastData classifiers) $ inFiles
      let (errorMessages, rendered) = partitionEithers $ toList results
      _ <- sequence errorMessages
      mapM_ TIO.putStrLn $ concat $ foldr mergeSortedEntries [] rendered
  where parseScript = first print . Lib.parseConfigFileText clF
        compileScript = first print . Lib.compileConfigFile (clF, 0, 0)
        parseBankData :: [T.Text] -> Lib.Classifiers -> String -> IO (Either (IO ()) [[T.Text]])
        parseBankData prev classifiers inF = do
          bankData <- readAndDecodeEitherXlsxOrText inF
          pure $ case bankData of
            Left rows -> parseArgentaBankData prev classifiers inF rows
            Right text -> parseBelfiusBankData prev classifiers inF text
        mergeSortedEntries :: [[T.Text]] -> [[T.Text]] -> [[T.Text]]
        mergeSortedEntries xs [] = xs
        mergeSortedEntries [] ys = ys
        mergeSortedEntries xx@(aa@(_:a:_):xs) yy@(bb@(_:b:_):ys)
          | a <= b = aa:mergeSortedEntries xs yy
          | otherwise = bb:mergeSortedEntries xx ys
        -- These five cases should never happen:
        mergeSortedEntries ([]:xs) ys = mergeSortedEntries xs ys
        mergeSortedEntries xs ([]:ys) = mergeSortedEntries xs ys
        mergeSortedEntries xx@(aa@[a]:xs) yy@(bb@[b]:ys)
          | a <= b = aa:mergeSortedEntries xs yy
          | otherwise = bb:mergeSortedEntries xx ys
        mergeSortedEntries (aa@[_]:xs) yy = aa:mergeSortedEntries xs yy
        mergeSortedEntries xx (bb@[_]:ys) = bb:mergeSortedEntries xx ys

readAndDecodeEitherXlsxOrText :: String -> IO (Either [XL.Row] T.Text)
readAndDecodeEitherXlsxOrText f = do
  bytes <- BL.readFile f
  case BL.take 4 bytes of
    -- Xlsx magic number = ZIP file magic numbers = file starts with "PK\x03\x04"
    "PK\ETX\EOT" -> Left <$> decodeXlsxToRows f
    _ -> Right <$> decodeToText f bytes

decodeXlsxToRows :: String -> IO [XL.Row]
decodeXlsxToRows inF = fmap (toListOf $ traversed . XL.si_row)
                          $ XL.runXlsxM inF $ XL.collectItems $ XL.makeIndex 1 -- 1st sheet

readAndDecodeText :: String -> IO T.Text
readAndDecodeText f = BL.readFile f >>= decodeToText f

decodeToText :: String -> BL.ByteString -> IO T.Text
decodeToText f bytes = do
  let (encoding, fileData) = case decodeUtf8' bytes of
        Left _ -> ("latin1", decodeLatin1 bytes)
        Right d -> ("utf8", d)
  putStrLn $ f <> " uses " <> encoding <> "."
  pure fileData

parseBelfiusBankData :: [T.Text] -- ^ existing ledger file lines
                     -> Lib.Classifiers -- ^ functions to map transactions to output text
                     -> String -- ^ bank data's file name (for error messages)
                     -> T.Text -- ^ bank data read from inF
                     -> Either (IO ()) [[T.Text]] -- ^ Either IO action to report the error or the list of transaction descriptions
parseBelfiusBankData pastLines c inF b = case Lib.runUnstructuredDataParser inF b of
    Left errorAction -> Left $ print errorAction
    Right belfiusData -> Right <$> filter isNotAlreadyThere
                                        $ map (uncurry $ renderTransaction c)
                                        $ reverse $ zip (Lib.getRawRows belfiusData)
                                                      $ Lib.columnsToBelfius belfiusData
  where pastData = extractPastRows pastLines
        isNotAlreadyThere (firstLine:_) = not $ firstLine `Set.member` pastData
        isNotAlreadyThere [] = True

parseArgentaBankData :: [T.Text] -- ^ existing ledger file lines
                     -> Lib.Classifiers -- ^ functions to map transactions to output text
                     -> String -- ^ bank data's file name (for error messages)
                     -> [XL.Row] -- ^ bank data read from inF
                     -> Either (IO ()) [[T.Text]] -- ^ Either IO action to report the error or the list of transaction descriptions
parseArgentaBankData pastLines classifiers inF rows = case Lib.parseXlsxRows rows of
    Left errMsg -> Left . print $ inF <> ": " <> errMsg
    Right argentaData -> Right <$> map (uncurry $ renderTransaction classifiers)
                                     $ reverse
                                     $ filter isNotAlreadyThere
                                     $ zip (Lib.getRawRows argentaData)
                                         $ Lib.columnsToArgenta argentaData
  where pastData = Set.fromList
                 $ filter (not . T.null)
                 $ map extractUniqueID
                 $ filter (prefixForRowAsComment `T.isPrefixOf`) pastLines
        extractUniqueID = safeFifth . T.splitOn ";"
        safeFifth (_:_:_:_:x:_) = x
        safeFifth _ = mempty
        isNotAlreadyThere (_, Right ad) = not $ (Lib.argentaReference ad) `Set.member` pastData
        isNotAlreadyThere (_, Left _) = True -- let errors percolate up

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
