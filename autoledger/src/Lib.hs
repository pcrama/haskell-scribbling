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
    packShow,
    packShow0Pad,
    parseAmountToCents,
    parseConfigFileText,
    parseUnstructuredData,
    parseUnstructuredDataRows,
    parseUnstructuredDataSingleRow,
    parseUnstructuredHeaderLine,
    parseUnstructuredHeaders,
    runUnstructuredDataParser,
    squeeze,
  )
where

import BelfiusParser
import Transaction
import ConfigLanguage
