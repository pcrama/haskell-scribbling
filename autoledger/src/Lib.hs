{-# LANGUAGE PatternSynonyms #-}

module Lib
  ( Classifiers (..),
    Compiled (..),
    ITransaction (..),
    LedgerEntry (..),
    TransactionEval (..),
    UnstructuredData (..),
    UnstructuredHeader (..),
    UnstructuredParser,
    UnstructuredParsingState,
    NonBlankText,
    pattern NonBlankText,
    evalForTransaction,
    Argenta.columnsToArgenta,
    columnsToBelfius,
    compileConfigFile,
    mkArgentaUnstructuredData,
    mkLedgerEntry,
    mkNonBlankText,
    packShow,
    packShow0Pad,
    parseAmountToCents,
    parseArgentaAmountToCents,
    parseConfigFileText,
    parseUnstructuredData,
    parseUnstructuredDataRows,
    parseUnstructuredDataSingleRow,
    parseUnstructuredHeaderLine,
    parseUnstructuredHeaders,
    Argenta.parseXlsxBS,
    runArgentaUnstructuredDataParser,
    runUnstructuredDataParser,
    squeeze,
  )
where

import Data.Text (Text)
import Text.Parsec (SourceName, ParseError, ParsecT)

import qualified ArgentaParser as Argenta
import BelfiusParser
import Transaction
import ConfigLanguage

mkArgentaUnstructuredData :: [Text]
  -> [(Int, [Text])]
  -> Argenta.UnstructuredData
mkArgentaUnstructuredData columnNames dataRows = Argenta.UnstructuredData {Argenta.udColumnNames = columnNames, Argenta.udData = dataRows}

parseArgentaAmountToCents :: Monad m => ParsecT Text () m Int
parseArgentaAmountToCents = Argenta.parseAmountToCents

runArgentaUnstructuredDataParser :: SourceName
  -> Text
  -> Either ParseError Argenta.UnstructuredData
runArgentaUnstructuredDataParser = Argenta.runUnstructuredDataParser
