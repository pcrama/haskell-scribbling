module ArgentaParser (
    UnstructuredData(..)
  , columnsToArgenta
  , parseAmountToCents -- for testing purposes
  , parseXlsxRows
  ) where
import Codec.Xlsx (Cell(..), CellValue(..), RowIndex(..), DateBase(..), cellValue, dateFromNumber)
import Codec.Xlsx.Parser.Stream (Row(..))
import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Functor (void)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Monoid (First(..), getFirst)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..), fromGregorian, toGregorian)
import Text.Parsec

import Transaction

parseXlsxRows :: [Row] -> Either String UnstructuredData
parseXlsxRows ((MkRow {_ri_row_index = RowIndex 1, _ri_cell_row = headerRow}):dataRows) = do
  headers <- collectOnlyCellText "Header row" (flip IntMap.lookup headerRow) 1 []
  pure $ UnstructuredData {udColumnNames = headers, udData = dataRows}
  where collectOnlyCellText :: String -> (Int -> Maybe Cell) -> Int -> [T.Text] -> Either String [T.Text]
        collectOnlyCellText errPrefix lookupCell currentCol reverseAcc =
          case lookupCell currentCol >>= (^. cellValue) of
            Just (CellText t) -> let nextCol = currentCol + 1
                                     nextAcc = t:reverseAcc in
                                   nextCol `seq` nextAcc `seq` collectOnlyCellText errPrefix lookupCell nextCol nextAcc
            Just c -> Left $ errPrefix <> ": wanted a text cell for " <> show currentCol <> ", got " <> show c
            Nothing -> pure $ reverse reverseAcc
parseXlsxRows _ = Left "Error: no data in first row?"

data UnstructuredData = UnstructuredData {
    udColumnNames :: [Text]
    , udData :: [Row] }
  deriving (Show)

instance IUnstructuredData UnstructuredData where
  getRawRows = map (\row -> (unRowIndex $ _ri_row_index row, [T.pack $ show row])) . udData

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

columnsToArgenta :: UnstructuredData -> [FailableToRecord ArgentaTransaction]
columnsToArgenta UnstructuredData { udColumnNames = columnNames, udData = dataRows } =
  map (makeArgentaPicking columnNames) dataRows

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

pickCellText :: Maybe T.Text -- ^ whether a (and which) default text can be substituted for Cell=Nothing or a Cell whose _cellValue=Nothing
              -> Filler T.Text a -- ^ a filler taking a cell text, to be transformed into a ...
              -> Filler (Maybe Cell) a -- ^ ... filler accepting a Cell (or Nothing) with runtime type checking
pickCellText Nothing _ Nothing _ = Left $ ColumnParsingError "Wanted a CellText, got nothing"
pickCellText (Just t) f Nothing r = f t r
pickCellText (Just t) f (Just (Cell { _cellValue = Nothing })) r = f t r
pickCellText _ f (Just (Cell { _cellValue = Just (CellText t) })) r = f t r
pickCellText _ _ (Just cell) _ = Left $ ColumnParsingError $ "Wanted a CellText, got a " <> (T.pack $ show cell)

pickCellDate :: Filler Day a -> Filler (Maybe Cell) a
pickCellDate _ Nothing _ = Left $ ColumnParsingError "Wanted a cell for a date, got nothing"
pickCellDate f (Just (Cell { _cellStyle = Just 2, _cellValue = Just (CellDouble d)})) r =
  let defaultDateBase = DateBase1900 -- TODO: detect whether we have DateBase1900 or DateBase1904
      utcTime = dateFromNumber defaultDateBase d in
    f (utctDay utcTime) r
pickCellDate _ (Just cell) _ = Left $ ColumnParsingError $ "Wanted a date, got " <> (T.pack $ show cell)

-- Boekdatum, Date de comptabilisation?
pickAccountingDate :: Filler (Maybe Cell) ArgentaTransaction
pickAccountingDate = pickCellDate pick
  where pick x r = pure $ r { _accountingDate = x }
-- Valutadatum, Date valeur?
pickValueDate :: Filler (Maybe Cell) ArgentaTransaction
pickValueDate = pickCellDate pick
  where pick x r = pure $ r { _valueDate = x }
-- Verrichtingsdatum, ?
pickTransactionDate :: Filler (Maybe Cell) ArgentaTransaction
pickTransactionDate = pickCellDate pick
  where pick x r = pure $ r { _transactionDate = x }
-- Rekening, Compte?
pickAccount :: Filler (Maybe Cell) ArgentaTransaction
pickAccount = pickCellText Nothing pick
  where pick x r = pure $ r { _account = x }

-- Referentie, ?
pickReference :: Filler (Maybe Cell) ArgentaTransaction
pickReference = pickCellText Nothing pick
  where pick x r = case mkNonBlankText x of
          Just nbt -> pure $ r { _reference = nbt }
          Nothing -> Left $ ColumnParsingError "Reference should not be blank"
-- Beschrijving, Transaction?
pickTransactionDescription :: Filler (Maybe Cell) ArgentaTransaction
pickTransactionDescription = pickCellText (Just "") pick
  where pick x r = pure $ r { _transactionDescription = mkNonBlankText x }
-- Rekening tegenpartij, Compte contrepartie?
pickOtherAccount :: Filler (Maybe Cell) ArgentaTransaction
pickOtherAccount = pickCellText (Just "") pick
  where pick x r = pure $ r { _otherAccount = mkNonBlankText x }
-- Naam tegenpartij, Nom contrepartie contient?
pickOtherName :: Filler (Maybe Cell) ArgentaTransaction
pickOtherName = pickCellText (Just "") pick
  where pick x r = pure $ r { _otherName = mkNonBlankText x }
-- Montant
pickAmountCents :: Filler (Maybe Cell) ArgentaTransaction
pickAmountCents Nothing _= Left $ ColumnParsingError $ "Unable to parse empty cell to an amount"
pickAmountCents (Just (Cell { _cellStyle = Just 1, _cellValue = Just (CellDouble d)})) r = pure $ r { _amountCents = round $ d * 100.0 }
pickAmountCents (Just (Cell { _cellValue = Just (CellText x)})) r = case runParser (parseAmountToCents <* eof) () "pickAmountCents" x of
  Left e -> Left $ ColumnParsingError $ "Unable to parse '" <> x <> "' to an amount: " <> T.pack (show e)
  Right cents -> pure $ r { _amountCents = cents }
pickAmountCents (Just cell) _ = Left $ ColumnParsingError $ "Unable to parse '" <> (T.pack $ show cell) <> "' to an amount."
-- Devise
pickCurrency :: Filler (Maybe Cell) ArgentaTransaction
pickCurrency = pickCellText (Just "") pick
  where pick x r = pure $ r { _currency = x }
-- Communications
pickCommunication :: Filler (Maybe Cell) ArgentaTransaction
pickCommunication = pickCellText (Just "") pick
  where pick x r = pure $ r { _communication = mkNonBlankText x }

picking :: [Filler i r] -> Filler [i] r
picking [] [] r = pure r
picking [] (_:_) _ = Left MoreDataColumnsThanHeaderColumns
picking (_:_) [] _ = Left MoreHeaderColumnsThanDataColumns
picking (f:fs) (x:xs) r = f x r >>= picking fs xs

makePicking ::
  (Text -> Maybe (Filler (Maybe Cell) r)) -- ^ lookup a filler for a given column name
  -> r -- ^ default record to be filled
  -> [Text] -- ^ column names
  -> (Row -> FailableToRecord r) -- ^ function mapping a data row to a filled record
makePicking lkp defaultRecord columnNames = case traverse selectPicker columnNames of
    Left unknownName -> const . Left $ UnknownColumnHeader unknownName
    Right fillers -> let columnLookups = map IntMap.lookup [1..length columnNames] in
                       \row -> let cellRow = _ri_cell_row row in
                         picking fillers (map ($ cellRow) columnLookups) defaultRecord
  where selectPicker h = maybe (Left h) Right $ lkp h

makeArgentaPicking ::
  -- | column names
  [Text] ->
  -- | function mapping a data row to a `ArgentaTransaction`
  (Row -> FailableToRecord ArgentaTransaction)
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
  where bilingualLookup :: T.Text -> Maybe (Filler (Maybe Cell) ArgentaTransaction)
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
