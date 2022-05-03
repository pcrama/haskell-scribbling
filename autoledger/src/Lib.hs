{-# LANGUAGE PatternSynonyms #-}

module Lib
  ( ITransaction (..),
    Classifiers (..),
    Compiled (..),
    LedgerEntry (..),
    TransactionEval (..),
    UnstructuredParser,
    UnstructuredParsingState,
    UnstructuredData (..),
    UnstructuredHeader (..),
    NonBlankText,
    pattern NonBlankText,
    evalForTransaction,
    columnsToBelfius,
    compileConfigFile,
    mkLedgerEntry,
    mkNonBlankText,
    parseAmountToCents,
    parseConfigFileText,
    parseUnstructuredData,
    parseUnstructuredDataRows,
    parseUnstructuredDataSingleRow,
    parseUnstructuredHeaderLine,
    parseUnstructuredHeaders,
    runUnstructuredDataParser,
  )
where

import BelfiusParser
import Transaction
import ConfigLanguage
