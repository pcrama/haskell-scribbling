module Main where
import qualified Data.ByteString as BL
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified Lib (runUnstructuredDataParser)

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
      Right (_headers, columnNames, rows) -> do
        case rows of
          [] -> print columnNames
          h:_ -> mapM_ (\(n, v) -> TIO.putStrLn $ n <> ": " <> v)
                     $ zip columnNames h
