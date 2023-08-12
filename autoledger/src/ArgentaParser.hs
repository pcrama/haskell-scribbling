module ArgentaParser (
  UnstructuredParser
  , UnstructuredParsingState
  , UnstructuredData(..)
  , columnsToArgenta
  , parseAmountToCents
  , parseUnstructuredData
  , parseUnstructuredDataRows
  , parseUnstructuredDataSingleRow
  , parseXlsxBS
  , runUnstructuredDataParser
  ) where
import Codec.Xlsx (Cell, CellValue(..), ColumnIndex(..), RowIndex(..), cellValue, toRows, toXlsxEither, wsCells, xlSheets)
import Control.Lens ((^.))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Functor (void)
import Data.List (foldl')
import Data.Monoid (First(..), getFirst)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian, fromGregorianValid, toGregorian)
import Text.Parsec

import Transaction

parseXlsxBS :: L.ByteString -> Either String UnstructuredData
parseXlsxBS bs = do
  xlsx <- mapError show $ toXlsxEither bs
  ws <-  getSingleWorksheet xlsx
  let cells = toRows $ ws ^. wsCells
  (headers, dataCells) <- splitHeaderAndData cells
  dataRows <- traverse extractDataRowTexts dataCells
  pure $ UnstructuredData {udColumnNames = headers, udData = dataRows}
  where getSingleWorksheet x =
          case x ^. xlSheets of
            [(_, worksheet)] -> pure worksheet
            [] -> Left "No worksheets in input file"
            _ -> Left "Too many worksheets in input file"
        splitHeaderAndData ((RowIndex 1, headerRow):dataRows) = do
          headers <- sequence $ zipWith (matchColumnIndexAndExtractCellText "Header row") [ColumnIndex 1..] headerRow
          pure (headers, dataRows)
        splitHeaderAndData (rowIdx:_) = Left $ "Wanted a header row at index 1, not " <> show rowIdx
        splitHeaderAndData [] = Left "Empty worksheet"
        tuple car cdr = (car, cdr)
        extractDataRowTexts :: (RowIndex, [(ColumnIndex, Cell)]) -> Either String (Int, [Text])
        extractDataRowTexts (RowIndex rowIdx, cells) =
          tuple rowIdx <$> (sequence $ zipWith (matchColumnIndexAndExtractCellText $ "Sheet row " <> show rowIdx) [ColumnIndex 1..] cells)
        matchColumnIndexAndExtractCellText :: String -> ColumnIndex -> (ColumnIndex, Cell) -> Either String T.Text
        matchColumnIndexAndExtractCellText errPrefix xpCi (obsCi, cell)
          | xpCi == obsCi = case cell ^. cellValue of
              Just (CellText t) -> pure t
              Just c -> Left $ errPrefix <> ": wanted a text cell for " <> show obsCi <> ", got " <> show c
              Nothing -> Left $ errPrefix <> ": wanted Just a text cell for " <> show obsCi <> ", got Nothing"
          | otherwise = Left $ errPrefix <> ": out of order columns or sparse row, got " <> show obsCi <> ", wanted " <> show xpCi
        mapError f (Left x) = Left $ f x
        mapError _ (Right x) = pure x
fieldSeparator :: Char
fieldSeparator = ','

newLines :: String
newLines = "\r\n"

doubleQuote :: Char
doubleQuote = '"'

ssvChar :: Char -> Bool
ssvChar = not . (`elem` (fieldSeparator:newLines))

ssvText :: Monad m => UnstructuredParser m Text
ssvText = T.pack <$> (
  (char doubleQuote *> many (satisfy $ not . (`elem` (doubleQuote:newLines))) <* char doubleQuote)
  <|> many (satisfy ssvChar))

type UnstructuredParsingState = Maybe Int
type UnstructuredParser m a = ParsecT Text UnstructuredParsingState m a

getColumnCount :: UnstructuredParsingState -> Maybe Int
getColumnCount = id

modifyColumnCount :: (Maybe Int -> Maybe Int) -> UnstructuredParsingState -> UnstructuredParsingState
modifyColumnCount = ($)

eol :: Monad m => UnstructuredParser m ()
eol = void endOfLine

parseUnstructuredDataSingleRow :: Monad m => UnstructuredParser m [Text]
parseUnstructuredDataSingleRow = sepBy1 ssvText (char fieldSeparator)

parseUnstructuredDataRows :: Monad m => UnstructuredParser m [(Int, [Text])]
parseUnstructuredDataRows = do
    Just colCount <- getColumnCount <$> getState
    line <- sourceLine <$> getPosition
    row <- parseUnstructuredDataSingleRow
    let rowLength = length row
    if rowLength == colCount
    then continueParsing line row
    else case reverse row of
           lastElt:butLast ->
             if rowLength == colCount + 1 && T.null lastElt
             then continueParsing line $ reverse butLast
             else fail $ "Got " <> show rowLength <> " row elements, but expected " <> show colCount <> "."
           [] -> fail $ "Got an empty row, but expected " <> show colCount <> " columns."
  where continueParsing lineNumber row = do
          ((lineNumber, row):) <$> (endOfInput <|> endOfLineAndNextRow)
        endOfInput = eof >> pure []
        endOfLineAndNextRow = eol >> (endOfInput <|> parseUnstructuredDataRows)

data UnstructuredData = UnstructuredData {
    udColumnNames :: [Text]
    , udData :: [(Int, [Text])] }
  deriving (Show, Eq)

parseUnstructuredData :: Monad m => UnstructuredParser m UnstructuredData
parseUnstructuredData = do
  columnNames <- parseUnstructuredDataSingleRow
  eol
  modifyState (modifyColumnCount $ const $ Just $ length columnNames)
  rows <- parseUnstructuredDataRows
  spaces
  eof
  pure $ UnstructuredData { udColumnNames = columnNames, udData = rows }

runUnstructuredDataParser :: SourceName
                          -> Text
                          -> Either ParseError UnstructuredData
runUnstructuredDataParser = runParser parseUnstructuredData Nothing

data ArgentaTransaction = ArgentaTransaction
  {
    _account :: Text -- Rekening, Compte?
  , _accountingDate :: Day -- Boekdatum, Date de comptabilisation?
  , _valueDate :: Day -- Valutadatum, Date valeur?
  , _reference :: NonBlankText -- Referentie, Numéro d'extrait?
  , _transactionDescription :: Maybe NonBlankText -- Beschrijving, Transaction?
  , _amountCents :: Int -- Bedrag, Montant?
  , _currency :: Text -- Munt, Devise?
  , _transactionDate :: Day -- Verrichtingsdatum, ?
  , _otherAccount :: Maybe NonBlankText -- Rekening tegenpartij, Compte contrepartie?
  , _otherName :: Maybe NonBlankText -- Naam tegenpartij, Nom contrepartie contient?
  , _communication :: Maybe NonBlankText -- Mededeling, Communications?
  }
  deriving (Show, Eq)

instance ITransaction ArgentaTransaction where
  account = _account
  date = _accountingDate
  otherAccount = _otherAccount
  otherName = _otherName
  description = getArgentaDescription
  amountCents = _amountCents
  currency = _currency
  identifyingComment = getArgentaIdentifyingComment

getArgentaDescription :: ArgentaTransaction -> Maybe NonBlankText
getArgentaDescription ArgentaTransaction { _communication = Nothing, _transactionDescription = mbT@(Just _) } =
  mbT >>= simplifyTransactionDescription
getArgentaDescription ArgentaTransaction { _communication = mbC@(Just (NonBlankText c)), _transactionDescription = mbT, _otherName = mbO }
  | isStructuredCommunication c = getFirst $
      First (mbT >>= simplifyTransactionDescription)
      <> First (joinNonBlankTextWith " " <$> mbO <*> mkNonBlankText c)
  | otherwise = mbC >>= simplifyTransactionDescription
  where isStructuredCommunication = T.all (\h -> isDigit h || h == '+' || h == '/') 
getArgentaDescription ArgentaTransaction { _communication = Nothing, _transactionDescription = Nothing, _otherName = Just on } = pure on
getArgentaDescription ArgentaTransaction { _communication = Nothing, _transactionDescription = Nothing, _otherName = Nothing } = Just uninitializedNonBlankText

simplifyTransactionDescription :: NonBlankText -> Maybe NonBlankText
simplifyTransactionDescription x = squeezeBlanks x
  >>= dropAchatBancontact
  >>= dropAchatContactLess
  >>= dropVirementMobile
  >>= dropAchatParInternet
  >>= dropPaiementViaApp
  >>= shortenArgentRecu
  >>= dropPaiementMaestro
  >>= dropRefVal
  where dropAchatBancontact = dropPrefix "ACHAT BANCONTACT AVEC CARTE N°" cardNumber
        dropAchatContactLess = dropPrefix "ACHAT BANCONTACT CONTACTLESS AVEC CARTE N°" cardNumber
        dropVirementMobile = dropPrefix "VIREMENT ARGENTA MOBILE VERS " $ const False
        dropAchatParInternet = dropPrefix "ACHAT PAR INTERNET AVEC CARTE N°" cardNumber
        argentRecuPrefix = "ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE BANCONTACT-APP LE"
        votreCarteBancairePattern = "SUR VOTRE CARTE BANCAIRE."
        shortenArgentRecu nb@(NonBlankText y)
          | argentRecuPrefix `T.isPrefixOf` y = let (before, after) = T.breakOn votreCarteBancairePattern y in
              if T.null after
              then return nb
              else mkNonBlankText $ "Argent reçu le" <> T.drop (T.length argentRecuPrefix) before <> "via votre app" <> T.drop (T.length votreCarteBancairePattern) after
          | otherwise = return nb
        dropPaiementViaApp = dropPrefix "PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE BANCONTACT-APP A " $ const False
        dropPaiementMaestro = dropPrefix "PAIEMENT MAESTRO " dayMonth
        dayMonth c = isDigit c || c == ' ' || c == '-' || c == '/'
        cardNumber c = isDigit c || c == ' ' || c == '-'
        patternRef = " REF. : "
        patternRefLen = T.length patternRef
        patternVal = " VAL. "
        patternValLen = T.length patternVal
        dropRefVal nb@(NonBlankText y) = let (prefix, s) = T.breakOn patternRef y in
          if T.null s
          then Just nb
          else let (t, suffix) = T.breakOn patternVal $ T.drop patternRefLen s in
                 case (T.null suffix,
                       not (T.any isSpace t)
                       && T.all cardNumber (T.drop patternValLen suffix)) of
                   (False, True) -> mkNonBlankText prefix
                   _ -> Just nb
        dropPrefix pfx extra nb@(NonBlankText y) = case T.stripPrefix pfx y of
          Just rest -> mkNonBlankText $ T.dropWhile extra rest
          Nothing -> Just nb
        squeezeBlanks (NonBlankText y) = mkNonBlankText $ squeeze y

parseUnsignedInt :: Monad m => ParsecT Text () m Int
parseUnsignedInt = digitListToInt <$> many1 (satisfy isSeparatorOrDigit)
  where digitListToInt = foldl' acc 0
        acc val '.' = val
        acc val dig = val * 10 + digitToInt dig
        isSeparatorOrDigit '.' = True
        isSeparatorOrDigit x = isDigit x

parseFractionalPart :: Monad m => ParsecT Text () m Int
parseFractionalPart = do
    void $ char ','
    combine <$> parseDigit <*> option 0 parseDigit
  where combine c1 c2 = 10 * c1 + c2
        parseDigit = digitToInt <$> satisfy isDigit

parseAmountToCents :: Monad m => ParsecT Text () m Int
parseAmountToCents = toCents <$> signParser <*> parseUnsignedInt <*> optionMaybe parseFractionalPart <* eof
  where toCents sign ip Nothing = sign * ip * 100
        toCents sign ip (Just fp) = sign * (100 * abs ip + fp)
        signParser = do
          s <- optionMaybe (char '-')
          return $ case s of
            Just _ -> (-1)
            Nothing -> 1

parseDate :: Monad m => ParsecT Text () m Day
parseDate = do
  day <- parseUnsignedInt
  void $ char '-' <|> char '/'
  month <- parseUnsignedInt
  void $ char '-' <|> char '/'
  year <- parseUnsignedInt
  maybe (fail "Invalid date") pure $ fromGregorianValid (fromIntegral year) month day

columnsToArgenta :: UnstructuredData -> [FailableToRecord ArgentaTransaction]
columnsToArgenta UnstructuredData { udColumnNames = columnNames, udData = dataRows } =
  map (makeArgentaPicking columnNames . snd) dataRows

getArgentaIdentifyingComment :: ArgentaTransaction -> Text
getArgentaIdentifyingComment at = (_account at
                                   <> ";" <> argentaRenderGregorian (_accountingDate at)
                                   <> ";" <> argentaRenderGregorian (_valueDate at)
                                   <> ";" <> let NonBlankText s = _reference at in s
                                   <> ";" <> renderNonBlankText (_transactionDescription at)
                                   <> ";" <> argentaRenderAmount (_amountCents at)
                                   <> ";" <> _currency at
                                   <> ";" <> argentaRenderGregorian (_transactionDate at)
                                   <> ";" <> renderNonBlankText (_otherAccount at)
                                   <> ";" <> renderNonBlankText (_otherName at)
                                   <> ";" <> renderNonBlankText (_communication at))

argentaRenderAmount :: Int -> T.Text
argentaRenderAmount amount = sign <> packShow units <> "," <> packShow0Pad 2 cents
  where (units, cents) = abs amount `divMod` 100
        sign = if amount < 0 then "-" else T.empty

argentaRenderGregorian :: Day -> T.Text
argentaRenderGregorian t = packShow0Pad 2 dt <> "/" <> packShow0Pad 2 mn <> "/" <> packShow yr
  where (yr, mn, dt) = toGregorian t

renderNonBlankText :: Maybe NonBlankText -> T.Text
renderNonBlankText Nothing = ""
renderNonBlankText (Just (NonBlankText t)) = t

-- Boekdatum, Date de comptabilisation?
pickAccountingDate :: Filler Text ArgentaTransaction
pickAccountingDate x r = case runParser (parseDate <* eof) () "pickAccountingDate" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to a date: " <> T.pack (show e)
  Right day -> pure $ r { _accountingDate = day }
-- Valutadatum, Date valeur?
pickValueDate :: Filler Text ArgentaTransaction
pickValueDate x r = case runParser (parseDate <* eof) () "pickValueDate" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to a date: " <> T.pack (show e)
  Right day -> pure $ r { _valueDate = day }
-- Verrichtingsdatum, ?
pickTransactionDate :: Filler Text ArgentaTransaction
pickTransactionDate x r = case runParser (parseDate <* eof) () "pickTransactionDate" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to a date: " <> T.pack (show e)
  Right day -> pure $ r { _transactionDate = day }
-- Rekening, Compte?
pickAccount :: Filler Text ArgentaTransaction
pickAccount x r = pure $ r { _account = x }
-- Referentie, ?
pickReference :: Filler Text ArgentaTransaction
pickReference x r = case mkNonBlankText x of
  Just nbt -> pure $ r { _reference = nbt }
  Nothing -> Left $ ColumnParsingError "Reference should not be blank"
-- Beschrijving, Transaction?
pickTransactionDescription :: Filler Text ArgentaTransaction
pickTransactionDescription x r = pure $ r { _transactionDescription = mkNonBlankText x }
-- Rekening tegenpartij, Compte contrepartie?
pickOtherAccount :: Filler Text ArgentaTransaction
pickOtherAccount x r = pure $ r { _otherAccount = mkNonBlankText x }
-- Naam tegenpartij, Nom contrepartie contient?
pickOtherName :: Filler Text ArgentaTransaction
pickOtherName x r = pure $ r { _otherName = mkNonBlankText x }
-- Montant
pickAmountCents :: Filler Text ArgentaTransaction
pickAmountCents x r = case runParser (parseAmountToCents <* eof) () "pickAmountCents" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to an amount: " <> T.pack (show e)
  Right cents -> pure $ r { _amountCents = cents }
-- Devise
pickCurrency :: Filler Text ArgentaTransaction
pickCurrency x r = pure $ r { _currency = x }
-- Communications
pickCommunication :: Filler Text ArgentaTransaction
pickCommunication x r = pure $ r { _communication = mkNonBlankText x }

picking :: [Filler i r] -> Filler [i] r
picking [] [] r = pure r
picking [] (_:_) _ = Left MoreDataColumnsThanHeaderColumns
picking (_:_) [] _ = Left MoreHeaderColumnsThanDataColumns
picking (f:fs) (x:xs) r = f x r >>= picking fs xs

makePicking ::
  (Text -> Maybe (Filler Text r)) -- ^ lookup a filler for a given column name
  -> r -- ^ default record to be filled
  -> [Text] -- ^ column names
  -> ([Text] -> FailableToRecord r) -- ^ function mapping a data row to a filled record
makePicking lkp defaultRecord columnNames = case traverse selectPicker columnNames of
    Left unknownName -> const . Left $ UnknownColumnHeader unknownName
    Right fillers -> \cols -> picking fillers cols defaultRecord
  where selectPicker h = maybe (Left h) Right $ lkp h

makeArgentaPicking ::
  -- | column names
  [Text] ->
  -- | function mapping a data row to a `ArgentaTransaction`
  ([Text] -> FailableToRecord ArgentaTransaction)
makeArgentaPicking = makePicking bilingualLookup $ ArgentaTransaction {
    _account = mempty
  , _accountingDate = fromGregorian 1970 1 1
  , _valueDate = fromGregorian 1970 1 1
  , _reference = uninitializedNonBlankText
  , _transactionDescription = Nothing
  , _amountCents = 0
  , _currency = mempty
  , _transactionDate = fromGregorian 1970 1 1
  , _otherAccount = Nothing
  , _otherName = Nothing
  , _communication = Nothing
  }
  where bilingualLookup :: T.Text -> Maybe (Filler T.Text ArgentaTransaction)
        bilingualLookup s =
          foldr (\(fr, nl, res) other -> if s == fr || s == nl then Just res else other)
                Nothing
                [
                  ("Compte?", "Rekening", pickAccount)
                , ("Date de comptabilisation?", "Boekdatum", pickAccountingDate)
                , ("Date valeur?", "Valutadatum", pickValueDate)
                , ("Numéro d'extrait?", "Referentie", pickReference)
                , ("Transaction?", "Beschrijving", pickTransactionDescription)
                , ("Montant?", "Bedrag", pickAmountCents)
                , ("Devise?", "Munt", pickCurrency)
                , ("?", "Verrichtingsdatum", pickTransactionDate)
                , ("Compte contrepartie?", "Rekening tegenpartij", pickOtherAccount)
                , ("Nom contrepartie contient?", "Naam tegenpartij", pickOtherName)
                , ("Communications?", "Mededeling", pickCommunication)
                ]
