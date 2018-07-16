module Events (
  Time
  , HostId
  , FileContent(..)
  , Operation(..)
  , Conflicts
  , Observation(..)
  )

where

import Control.Concurrent (threadDelay)
import Control.Exception (tryJust)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (
  ReaderT
  , asks
  , liftIO
  )
import Data.Word (Word16)
import System.Directory (removeFile)
import System.IO.Error (tryIOError)

newtype Time = TimeMs Word16
  deriving (Show)

newtype HostId = HostId Int
  deriving (Eq, Show)

data FileContent
    = FileContent String
    | FileEmpty
  deriving (Eq, Show)

data Operation
    = OpRead HostId
    | OpWrite HostId FileContent
    | OpSleep Time
    | OpStabilize
  deriving (Show)

data StabilizationResult
    = StabOK FileContent Conflicts
    | StabFail [(HostId, FileContent, Conflicts)]

class HasConfig c where
  filePathUnderTest :: c -> HostId -> FilePath

class (Monad m) => SynctestIO m where
  doRead :: HasConfig c => HostId -> ReaderT c m FileContent
  doWrite :: HasConfig c => HostId -> FileContent -> ReaderT c m FileContent
  doSleep :: Time -> ReaderT c m ()
  doStabilize :: HasConfig c => ReaderT c m StabilizationResult

type Conflicts = [(HostId, FileContent)]

data Observation
    = ObsRead HostId FileContent
    | ObsWrite HostId FileContent
    | ObsSleep 
    | ObsStabilize FileContent Conflicts
    | ObsFailedStabilize [(HostId, FileContent, Conflicts)]
  deriving (Show)

performOperation :: (HasConfig c, SynctestIO m) => Operation -> ReaderT c m Observation
performOperation (OpRead hId) = do
  c <- doRead hId
  return $ ObsRead hId c
performOperation (OpWrite hId fc) = do
  oldContent <- doRead hId
  doWrite hId fc
  return $ ObsWrite hId oldContent
performOperation (OpSleep time) = doSleep time >> return ObsSleep
performOperation OpStabilize = do
  stab <- doStabilize
  return $ case stab of
             StabOK fc cs -> ObsStabilize fc cs
             StabFail csMap -> ObsFailedStabilize csMap

askFilePathUnderTest :: (HasConfig c, Monad m) => HostId -> ReaderT c m FilePath
askFilePathUnderTest hostId = do
  fpFun <- asks filePathUnderTest
  return $ fpFun hostId

lowLevelReadIO :: (MonadIO m, HasConfig c) => HostId -> ReaderT c m (FilePath, FileContent)
lowLevelReadIO hostId = do
  fp <- askFilePathUnderTest hostId
  c' <- liftIO $ tryIOError $ readFile fp
  return $ (fp
           , case c' of
               Left _ -> FileEmpty
               Right c -> c `seq` FileContent c)
  
doReadIO :: (MonadIO m, HasConfig c) => HostId -> ReaderT c m FileContent
doReadIO = fmap snd . lowLevelReadIO
  
doWriteIO :: (MonadIO m, HasConfig c) => HostId -> FileContent -> ReaderT c m FileContent
doWriteIO hostId fc = do
  (fp, oldContent) <- lowLevelReadIO hostId
  r <- liftIO $ tryIOError $ doWriteOrDelete fp fc
  case r of
    -- TODO: decide how to make this function total
    Left err -> error $ "doWriteIO " ++ show hostId ++ ": error writing to " ++ fp ++ ": " ++ show err
    Right () -> return oldContent
  where
    doWriteOrDelete fp FileEmpty = removeFile fp
    doWriteOrDelete fp (FileContent newContent) = writeFile fp newContent
  
doSleepIO :: MonadIO m => Time -> ReaderT c m ()
doSleepIO (TimeMs ms) = liftIO $ threadDelay $ fromIntegral ms

-- TODO: when logfile of unisonsync.sh call isn't updated/created
-- anymore, the syncing sees no new data to transfer (since
-- 05bf67e5a1227a0bf220bc972f139f16900d08f9).  Detect this with a
-- timeout...
--
-- As long as a conflict remains, the logfile with be updated with the
-- information of that conflict.
doStabilizeIO :: MonadIO m => HasConfig c => ReaderT c m StabilizationResult
doStabilizeIO = undefined

-- (Monad m, MonadIO m) => SynctestIO m would require
-- {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- and I'm not sure it's worth it.
instance SynctestIO IO where
  doRead = doReadIO
  doWrite = doWriteIO
  doSleep = doSleepIO
  doStabilize = doStabilizeIO
  
