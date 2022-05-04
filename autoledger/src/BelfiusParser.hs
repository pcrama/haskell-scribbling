module BelfiusParser (
  UnstructuredParser
  , UnstructuredParsingState
  , UnstructuredData(..)
  , UnstructuredHeader(..)
  , columnsToBelfius
  , parseAmountToCents
  , parseUnstructuredData
  , parseUnstructuredDataRows
  , parseUnstructuredDataSingleRow
  , parseUnstructuredHeaderLine
  , parseUnstructuredHeaders
  , runUnstructuredDataParser
  ) where
import qualified Data.Text as T
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Functor (void)
import Data.List (foldl')
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian, fromGregorianValid)
import Text.Parsec
import Text.Read (readMaybe)

import Transaction

data UnstructuredHeader = UnstructuredHeader {
      uhLine :: Int
    , uhKey :: Text
    , uhValue :: Text
    }
  deriving (Show, Eq)

fieldSeparator :: Char
fieldSeparator = ';'

newLines :: String
newLines = "\r\n"

ssvChar :: Char -> Bool
ssvChar = not . (`elem` (fieldSeparator:newLines))

ssvText, ssvText1 :: Monad m => UnstructuredParser m Text
ssvText = T.pack <$> many (satisfy ssvChar)
ssvText1 = T.pack <$> many1 (satisfy ssvChar)

type UnstructuredParsingState = Maybe Int
type UnstructuredParser m a = ParsecT Text UnstructuredParsingState m a

getColumnCount :: UnstructuredParsingState -> Maybe Int
getColumnCount = id

modifyColumnCount :: (Maybe Int -> Maybe Int) -> UnstructuredParsingState -> UnstructuredParsingState
modifyColumnCount = ($)

eol :: Monad m => UnstructuredParser m ()
eol = void endOfLine

parseUnstructuredHeaderLine :: Monad m => UnstructuredParser m UnstructuredHeader
parseUnstructuredHeaderLine = do
    spaces
    key <- ssvText1
    _ <- char fieldSeparator
    position <- getPosition
    value <- ssvText
    return $ UnstructuredHeader {
        uhLine = sourceLine position
      , uhKey = T.stripEnd key
      , uhValue = T.strip value
      }

parseUnstructuredHeaders :: Monad m => UnstructuredParser m [UnstructuredHeader]
parseUnstructuredHeaders =
    manyTill (parseUnstructuredHeaderLine <* eol) (char fieldSeparator >> eol)

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
    else let lastElt:butLast = reverse row in
         if rowLength == colCount + 1 && T.null lastElt
         then continueParsing line $ reverse butLast
         else fail $ "Got " <> show rowLength <> " row elements, but expected " <> show colCount <> "."
  where continueParsing lineNumber row = do
          ((lineNumber, row):) <$> (endOfInput <|> endOfLineAndNextRow)
        endOfInput = eof >> pure []
        endOfLineAndNextRow = eol >> (endOfInput <|> parseUnstructuredDataRows)

data UnstructuredData = UnstructuredData {
      udHeaders :: [UnstructuredHeader]
    , udColumnNames :: [Text]
    , udData :: [(Int, [Text])] }
  deriving (Show, Eq)

parseUnstructuredData :: Monad m => UnstructuredParser m UnstructuredData
parseUnstructuredData = do
  headers <- parseUnstructuredHeaders
  columnNames <- parseUnstructuredDataSingleRow
  eol
  modifyState (modifyColumnCount $ const $ Just $ length columnNames)
  rows <- parseUnstructuredDataRows
  spaces
  eof
  pure $ UnstructuredData { udHeaders = headers, udColumnNames = columnNames, udData = rows }

runUnstructuredDataParser :: SourceName
                          -> Text
                          -> Either ParseError UnstructuredData
runUnstructuredDataParser = runParser parseUnstructuredData Nothing

data BelfiusTransaction = BelfiusTransaction
  {
    _account :: Text -- Compte
  , _accountingDate :: Day -- Date de comptabilisation
  , _extractNumber :: Maybe Int -- Numéro d'extrait
  , _transactionNumber :: Maybe Int -- Numéro de transaction
  , _otherAccount :: Maybe NonBlankText -- Compte contrepartie
  , _otherName :: Maybe NonBlankText -- Nom contrepartie contient
  , _otherStreetAndNumber :: Maybe NonBlankText -- Rue et numéro
  , _otherCity :: Maybe NonBlankText -- Code postal et localité
  , _transactionDescription :: Maybe NonBlankText -- Transaction
  , _valueDate :: Day -- Date valeur
  , _amountCents :: Int -- Montant
  , _currency :: Text -- Devise
  , _bankIdentificationCode :: Text -- BIC
  , _countryCode :: Text -- Code pays
  , _communication :: Maybe NonBlankText -- Communications
  }
  deriving (Show, Eq)

instance ITransaction BelfiusTransaction where
  account = _account
  date = _accountingDate
  otherAccount = _otherAccount
  otherName = _otherName
  description = getBelfiusDescription
  amountCents = _amountCents
  currency = _currency

getBelfiusDescription :: BelfiusTransaction -> Maybe NonBlankText
getBelfiusDescription BelfiusTransaction { _communication = Nothing, _transactionDescription = mbT@(Just _) } =
  mbT >>= simplifyTransactionDescription
getBelfiusDescription BelfiusTransaction { _communication = mbC@(Just (NonBlankText c)), _transactionDescription = mbT }
  | isStructuredCommunication c = mbT >>= simplifyTransactionDescription
  | otherwise = mbC >>= simplifyTransactionDescription
  where isStructuredCommunication = T.all (\h -> isDigit h || h == '+' || h == '/') 
getBelfiusDescription BelfiusTransaction { _communication = Nothing, _transactionDescription = Nothing } = Nothing

simplifyTransactionDescription :: NonBlankText -> Maybe NonBlankText
simplifyTransactionDescription x = dropAchatBancontact x
  >>= dropAchatContactLess
  >>= dropVirementMobile
  >>= dropAchatParInternet
  >>= dropPaiementViaApp
  >>= dropPaiementMaestro
  >>= dropRefVal
  where dropAchatBancontact = dropPrefix "ACHAT BANCONTACT AVEC CARTE N°" cardNumber
        dropAchatContactLess = dropPrefix "ACHAT BANCONTACT CONTACTLESS AVEC CARTE N°" cardNumber
        dropVirementMobile = dropPrefix "VIREMENT BELFIUS MOBILE VERS " $ const False
        dropAchatParInternet = dropPrefix "ACHAT PAR INTERNET AVEC CARTE N°" cardNumber
        dropPaiementViaApp = dropPrefix "PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A " $ const False
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

parseUnsignedInt :: Monad m => ParsecT Text () m Int
parseUnsignedInt = digitListToInt <$> many1 (satisfy isDigit)
  where digitListToInt = foldl' (\val dig -> val * 10 + digitToInt dig) 0

parseFractionalPart :: Monad m => ParsecT Text () m Int
parseFractionalPart = do
    void $ char '.' <|> char ','
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

data UnstructuredDataToRecordError a =
  MoreDataColumnsThanHeaderColumns
  | MoreHeaderColumnsThanDataColumns
  | UnknownColumnHeader Text
  | ColumnParsingError a
  deriving (Show, Eq)

type FailableToRecord a = Either (UnstructuredDataToRecordError Text) a

columnsToBelfius :: UnstructuredData -> [FailableToRecord BelfiusTransaction]
columnsToBelfius UnstructuredData { udColumnNames = columnNames, udData = dataRows } =
  map (makeBelfiusPicking columnNames . snd) dataRows

type Filler i r = i -> r -> Either (UnstructuredDataToRecordError Text) r

mkNonBlankInt :: Text -> (Maybe Int -> a) -> FailableToRecord a
mkNonBlankInt t setter
  | T.all isSpace t = Right $ setter Nothing
  | otherwise = maybe (Left $ ColumnParsingError $ "Can't parse '" <> t <> "' into a number.")
                      (Right . setter . Just)
                    $ readMaybe $ T.unpack t

-- Date de comptabilisation
pickAccountingDate :: Filler Text BelfiusTransaction
pickAccountingDate x r = case runParser (parseDate <* eof) () "pickAccountingDate" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to a date: " <> T.pack (show e)
  Right day -> pure $ r { _accountingDate = day }
-- Date valeur
pickValueDate :: Filler Text BelfiusTransaction
pickValueDate x r = case runParser (parseDate <* eof) () "pickValueDate" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to a date: " <> T.pack (show e)
  Right day -> pure $ r { _valueDate = day }
-- Compte
pickAccount :: Filler Text BelfiusTransaction
pickAccount x r = pure $ r { _account = x }
-- Numéro d'extrait
pickExtractNumber :: Filler Text BelfiusTransaction
pickExtractNumber x r = mkNonBlankInt x $ \mbi -> r { _extractNumber = mbi }
-- Numéro de transaction
pickTransactionNumber :: Filler Text BelfiusTransaction
pickTransactionNumber x r = mkNonBlankInt x $ \mbi -> r { _transactionNumber = mbi }
-- Compte contrepartie
pickOtherAccount :: Filler Text BelfiusTransaction
pickOtherAccount x r = pure $ r { _otherAccount = mkNonBlankText x }
-- Nom contrepartie contient
pickOtherName :: Filler Text BelfiusTransaction
pickOtherName x r = pure $ r { _otherName = mkNonBlankText x }
-- Rue et numéro
pickOtherStreetAndNumber :: Filler Text BelfiusTransaction
pickOtherStreetAndNumber x r = pure $ r { _otherStreetAndNumber = mkNonBlankText x }
-- Code postal et localité
pickOtherCity :: Filler Text BelfiusTransaction
pickOtherCity x r = pure $ r { _otherCity = mkNonBlankText x }
-- Transaction
pickTransactionDescription :: Filler Text BelfiusTransaction
pickTransactionDescription x r = pure $ r { _transactionDescription = mkNonBlankText x }
-- Montant
pickAmountCents :: Filler Text BelfiusTransaction
pickAmountCents x r = case runParser (parseAmountToCents <* eof) () "pickAmountCents" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to an amount: " <> T.pack (show e)
  Right cents -> pure $ r { _amountCents = cents }
-- Devise
pickCurrency :: Filler Text BelfiusTransaction
pickCurrency x r = pure $ r { _currency = x }
-- BIC
pickBankIdentificationCode :: Filler Text BelfiusTransaction
pickBankIdentificationCode x r = pure $ r { _bankIdentificationCode = x }
-- Code pays
pickCountryCode :: Filler Text BelfiusTransaction
pickCountryCode x r = pure $ r { _countryCode = x }
-- Communications
pickCommunication :: Filler Text BelfiusTransaction
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
  where selectPicker h = case lkp h of
          Nothing -> Left h
          Just f -> Right $ f . T.unwords . T.words

makeBelfiusPicking ::
  -- | column names
  [Text] ->
  -- | function mapping a data row to a `BelfiusTransaction`
  ([Text] -> FailableToRecord BelfiusTransaction)
makeBelfiusPicking = makePicking bilingualLookup $ BelfiusTransaction {
  _account = T.empty
  , _accountingDate = fromGregorian 1970 1 1
  , _extractNumber = Nothing
  , _transactionNumber = Nothing
  , _otherAccount = Nothing
  , _otherName = Nothing
  , _otherStreetAndNumber = Nothing
  , _otherCity = Nothing
  , _transactionDescription = Nothing
  , _valueDate = fromGregorian 1970 1 1
  , _amountCents = 0
  , _currency = T.empty
  , _bankIdentificationCode = T.empty
  , _countryCode = T.empty
  , _communication = Nothing
  }
  where bilingualLookup :: T.Text -> Maybe (Filler T.Text BelfiusTransaction)
        bilingualLookup s =
          foldr (\(fr, nl, res) other -> if s == fr || s == nl then Just res else other)
                Nothing
                [("Communications", "Mededelingen", pickCommunication),
                 ("Compte", "Rekening", pickAccount),
                 ("Date de comptabilisation", "Boekingsdatum", pickAccountingDate),
                 ("Numéro d'extrait", "Rekeninguittrekselnummer", pickExtractNumber),
                 ("Numéro de transaction", "Transactienummer", pickTransactionNumber),
                 ("Compte contrepartie", "Rekening tegenpartij", pickOtherAccount),
                 ("Nom contrepartie contient", "Naam tegenpartij bevat", pickOtherName),
                 ("Rue et numéro", "Straat en nummer", pickOtherStreetAndNumber),
                 ("Code postal et localité", "Postcode en plaats", pickOtherCity),
                 ("Transaction", "Transactie", pickTransactionDescription),
                 ("Date valeur", "Valutadatum", pickValueDate),
                 ("Montant", "Bedrag", pickAmountCents),
                 ("Devise", "Devies", pickCurrency),
                 ("BIC", "BIC", pickBankIdentificationCode),
                 ("Code pays", "Landcode", pickCountryCode)]
