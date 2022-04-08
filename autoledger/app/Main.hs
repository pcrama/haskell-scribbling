module Main where

import qualified Data.ByteString as BL
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import qualified Data.Text.IO as TIO
import Data.Time (toGregorian)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Lib
  ( ITransaction (..),
    LedgerEntry (..),
    TransactionEval (..),
    UnstructuredData (..),
    columnsToBelfius,
    mkLedgerEntry,
    runUnstructuredDataParser,
  )
import System.Environment (getArgs)

squeeze :: T.Text -> T.Text
squeeze = T.unwords . T.words

newline :: T.Text
newline = "\n"

copyRowAsComment :: T.Text -> T.Text
copyRowAsComment x = ";<-" <> x

classifyAsset :: Lib.TransactionEval T.Text
classifyAsset = Lib.Select "NotReached" lkp Lib.Account
  where
    lkp s = case s of
      "BE12 3456 7890 1234" -> Just "Assets:BankAccount"
      _ -> Just s

classifyTransaction :: Lib.TransactionEval (T.Text, T.Text)
classifyTransaction =
  Lib.Cond
    (Lib.Pair Lib.Description $ Lib.Constant "Expenses:")
    [ ( Lib.Description `Lib.ContainsCaseInsensitive` Lib.Constant "titres service",
        Lib.Pair Lib.Description $ Lib.Constant "Expenses:Services:Entretien:Nettoyage"
      ),
      ( Lib.Description `Lib.ContainsCaseInsensitive` Lib.Constant "pepiniere",
        Lib.Pair Lib.Description $ Lib.Constant "Expenses:Jardin"
      )
    ]

classifyLedgerText :: Lib.TransactionEval T.Text
classifyLedgerText = Lib.Fst classifyTransaction

classifyLedgerOtherAccount :: Lib.TransactionEval T.Text
classifyLedgerOtherAccount = Lib.Snd classifyTransaction

packShow :: Show b => b -> T.Text
packShow = T.pack . show

packShow0Pad :: (Show b, Integral b) => Int -> b -> T.Text
packShow0Pad digits x =
  let shown = show x
      extraZeros = digits - length shown
   in if extraZeros > 0
        then T.replicate extraZeros "0" <> T.pack shown
        else T.pack shown

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
      renderedAmount = sign <> packShow units <> "." <> packShow0Pad 2 cents
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
renderDay t = packShow year <> "/" <> packShow0Pad 2 month <> "/" <> packShow0Pad 2 date
  where
    (year, month, date) = toGregorian $ Lib.ledgerDate t

renderDescription :: Lib.LedgerEntry -> T.Text
renderDescription = squeeze . Lib.ledgerText

renderTransaction ::
  (Show a, Lib.ITransaction r) =>
  (Int, [T.Text]) ->
  Either a r ->
  T.Text
renderTransaction (lineNo, dataRow) (Left err) =
  T.intercalate newline $
    [ copyRowAsComment $ T.intercalate ";" dataRow,
      "; error in line " <> T.pack (show lineNo)
    ]
      ++ map (T.pack . ("; " <>)) (lines $ show err)
      ++ [""]
renderTransaction (_, dataRow) (Right transaction) =
  renderLedgerEntry $
    Lib.mkLedgerEntry
      classifyLedgerText
      classifyAsset
      classifyLedgerOtherAccount
      dataRow
      transaction

renderLedgerEntry :: Lib.LedgerEntry -> T.Text
renderLedgerEntry le =
  T.intercalate newline $
    [ copyRowAsComment $ Lib.precedingComment le,
      renderDay le <> " " <> renderDescription le
    ]
      ++ renderAccountUpdate le
      -- Add trailing newline with [""]
      ++ [""]

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  let inputFile = case args of
        [f] -> f
        _ -> "script-input.txt"
  bankDataBytes <- BL.readFile inputFile
  let (encoding, bankData) = case decodeUtf8' bankDataBytes of
        Left _ -> ("latin1", decodeLatin1 bankDataBytes)
        Right d -> ("utf8", d)
  putStrLn $ inputFile <> " uses " <> encoding <> "."
  case Lib.runUnstructuredDataParser inputFile bankData of
    Left e -> print e
    Right ud -> do
      case Lib.udData ud of
        [] -> print $ Lib.udColumnNames ud
        _ -> mapM_ (TIO.putStrLn . uncurry renderTransaction) (reverse $ zip (Lib.udData ud) $ Lib.columnsToBelfius ud)

-- Local Variables:
-- compile-command: "(cd autoledger; cabal new-run exe:autoledger)"
-- coding: utf-8
-- End:
