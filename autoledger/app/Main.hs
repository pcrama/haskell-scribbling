{-# LANGUAGE PatternSynonyms #-}
module Main where
import qualified Data.ByteString as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Data.Time (toGregorian)
import System.Environment (getArgs)

import qualified Lib (
  ITransaction(..)
  , UnstructuredData(..)
  , pattern NonBlankText
  , columnsToBelfius
  , runUnstructuredDataParser
  )

squeeze :: T.Text -> T.Text
squeeze = T.unwords . T.words

newline :: T.Text
newline = "\n"

copyRowAsComment :: [T.Text] -> T.Text
copyRowAsComment = (";<-" <>) . T.intercalate ";"

classifyAsset :: Lib.ITransaction a => a -> T.Text
classifyAsset t = case Lib.account t of
  "BE12 3456 7890 1234" -> "Assets:BankAccount"
  s -> s

packShow :: Show b => b -> T.Text
packShow = T.pack . show

packShow0Pad :: (Show b, Integral b) => Int -> b -> T.Text
packShow0Pad digits x =
  let shown = show x
      extraZeros = digits - length shown in
    if extraZeros > 0
    then T.replicate extraZeros "0" <> T.pack shown
    else T.pack shown

renderAccountUpdate ::
  T.Text -- ^ account
  -> Int -- ^ amount in cents
  -> T.Text -- ^ currency
  -> T.Text
renderAccountUpdate account amountCents currency =
  let prefix = "    "
      target = 52
      left = prefix <> account
      (units, cents) = abs amountCents `divMod` 100
      sign = if amountCents < 0 then "-" else mempty
      renderedAmount = sign <> packShow units <> "." <> packShow0Pad 2 cents
      extraSpaces = max 2 $ target - T.length left - T.length renderedAmount
      currencySymbol = case currency of
        "EUR" -> "€"
        _ -> currency in
    left <> T.replicate extraSpaces " " <> renderedAmount <> " " <> currencySymbol

renderDay :: Lib.ITransaction a => a -> T.Text
renderDay t = packShow year <> "/" <> packShow0Pad 2 month <> "/" <> packShow0Pad 2 date
  where (year, month, date) = toGregorian $ Lib.date t

renderDescription :: Lib.ITransaction a => a -> T.Text
renderDescription t = case Lib.description t of
  Just (Lib.NonBlankText d) -> squeeze d
  Nothing -> "?"

renderTransaction :: (Show a, Lib.ITransaction r) =>
                     (Int, [T.Text]) -> Either a r -> T.Text
renderTransaction (lineNo, dataRow) (Left err) =
  T.intercalate newline
              $ [copyRowAsComment dataRow
                , "; error in line " <> (T.pack $ show lineNo)
                ] ++ map (T.pack . ("; " <>)) (lines $ show err)
                ++ [""]
renderTransaction (_, dataRow) (Right transaction) =
  T.intercalate newline
              $ [copyRowAsComment dataRow
                 , renderDay transaction <> " " <> renderDescription transaction
                 , renderAccountUpdate (classifyAsset transaction) (Lib.amountCents transaction) (Lib.currency transaction)
                 , ""]

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
-- compile-command: "([ -r autoledger.cabal ] || cd ..; cabal new-run exe:autoledger)"
-- coding: utf-8
-- End:
