module Main where
import qualified Data.ByteString as BL
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified Lib (
  UnstructuredData(..)
  , columnsToBelfius
  , runUnstructuredDataParser
  )

main :: IO ()
main = do
    setLocaleEncoding utf8
    let inputFile = "script-input.txt"
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
          _ -> flip mapM_ (take 5 $ Lib.columnsToBelfius ud) $ \h -> do
                      putStrLn "---"
                      print h

-- Local Variables:
-- compile-command: "(cd .. && cabal new-run exe:autoledger)"
-- End:
