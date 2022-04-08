{-# LANGUAGE PatternSynonyms #-}

module Lib
  ( ITransaction (..),
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
    mkLedgerEntry,
    mkNonBlankText,
    parseAmountToCents,
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
