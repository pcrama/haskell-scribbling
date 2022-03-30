{-# LANGUAGE PatternSynonyms #-}
module Lib (
  ITransaction(..)
  , TransactionEval(..)
  , UnstructuredParser
  , UnstructuredParsingState
  , UnstructuredData(..)
  , UnstructuredHeader(..)
  , NonBlankText
  , pattern NonBlankText
  , evalForTransaction
  , columnsToBelfius
  , mkNonBlankText
  , parseAmountToCents
  , parseUnstructuredData
  , parseUnstructuredDataRows
  , parseUnstructuredDataSingleRow
  , parseUnstructuredHeaderLine
  , parseUnstructuredHeaders
  , runUnstructuredDataParser
  ) where

import Transaction
import BelfiusParser
