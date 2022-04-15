{-# LANGUAGE PatternSynonyms #-}

module Lib
  ( ITransaction (..),
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
    compile,
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
