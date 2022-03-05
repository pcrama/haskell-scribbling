module Lib (
  UnstructuredParser
  , UnstructuredParsingState
  , UnstructuredData(..)
  , UnstructuredHeader(..)
  , parseUnstructuredData
  , parseUnstructuredDataRows
  , parseUnstructuredDataSingleRow
  , parseUnstructuredHeaderLine
  , parseUnstructuredHeaders
  , runUnstructuredDataParser
  ) where
import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Char (
  endOfLine
  , spaces
  , satisfy
  )

data UnstructuredHeader = UnstructuredHeader {
      uhLine :: Int
    , uhKey :: Text
    , uhValue :: Text
    }
  deriving (Show, Eq)

data UnstructuredData = UnstructuredData {
      udHeaders :: [Text]
    , udHeaderLine :: Int
    , udData :: [[Text]] }
  deriving (Show, Eq)

fieldSeparator :: Char
fieldSeparator = ';'

newLines :: String
newLines = "\r\n"

ssvChar :: Char -> Bool
ssvChar = (not . (`elem` (fieldSeparator:newLines)))

ssvText, ssvText1 :: Monad m => UnstructuredParser m Text
ssvText = T.pack <$> (many $ satisfy ssvChar)
ssvText1 = T.pack <$> (many1 $ satisfy ssvChar)

type UnstructuredParsingState = Maybe Int
type UnstructuredParser m a = ParsecT Text UnstructuredParsingState m a

getColumnCount :: UnstructuredParsingState -> Maybe Int
getColumnCount = id

modifyColumnCount :: (Maybe Int -> Maybe Int) -> UnstructuredParsingState -> UnstructuredParsingState
modifyColumnCount = ($)

eol :: Monad m => UnstructuredParser m ()
eol = endOfLine >> return ()

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

parseUnstructuredDataRows :: Monad m => UnstructuredParser m [[Text]]
parseUnstructuredDataRows = do
    Just colCount <- getColumnCount <$> getState
    row <- parseUnstructuredDataSingleRow
    let rowLength = length row
    if rowLength == colCount
    then continueParsing row
    else let lastElt:butLast = reverse row in
         if rowLength == colCount + 1 && T.null lastElt
         then continueParsing $ reverse butLast
         else fail $ "Got " <> show rowLength <> " row elements, but expected " <> show colCount <> "."
  where continueParsing row = do
          (row:) <$> (endOfInput <|> endOfLineAndNextRow)
        endOfInput = eof >> pure []
        endOfLineAndNextRow = eol >> (endOfInput <|> parseUnstructuredDataRows)

parseUnstructuredData :: Monad m => UnstructuredParser m ([UnstructuredHeader], [Text], [[Text]])
parseUnstructuredData = do
  headers <- parseUnstructuredHeaders
  columnNames <- parseUnstructuredDataSingleRow
  eol
  modifyState (modifyColumnCount $ const $ Just $ length columnNames)
  rows <- parseUnstructuredDataRows
  spaces
  eof
  pure (headers, columnNames, rows)

runUnstructuredDataParser :: SourceName
                          -> Text
                          -> Either ParseError ([UnstructuredHeader], [Text], [[Text]])
runUnstructuredDataParser = runParser parseUnstructuredData Nothing
