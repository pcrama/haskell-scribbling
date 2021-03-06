{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Events (
  Time
  , HostId(..)
  , FileContent(..)
  , Operation(..)
  , Conflicts
  , Observation(..)
  , Scenario(..)
  , performScenario
  , sample
  , Gen
  , arbitrary
  , minCfg
  , SimplisticConfig
  , HasConfig(..)
  , runReaderT
  )

where

import Debug.Trace (trace)
import Control.Concurrent (threadDelay)
import Control.Exception (tryJust)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (
  MonadReader
  , runReaderT
  , ReaderT
  , asks
  , liftIO
  )
import Data.Time.Clock (
  DiffTime
  , UTCTime(..)
  , getCurrentTime
  )
import Data.Word (Word16)
import System.Directory (
  doesFileExist
  , getModificationTime
  , removeFile
  )
import System.FilePath (
  FilePath
  , (</>)
  , takeDirectory
  , takeFileName
  )
import System.IO.Error (tryIOError)
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

newtype Time = TimeMs Word16
  deriving (Show)

instance Arbitrary Time where
  arbitrary = do
    let delaysInMs = [10, 100, 500]
    idx <- choose (0, length delaysInMs - 1)
    return $ TimeMs $ delaysInMs !! idx

data HostId = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary HostId where
  shrink h
    | h == minBound = []
    | fromEnum h > 2 = [pred h, pred $ pred h]
    | fromEnum h > 1 = [pred h]
    | otherwise = [] -- also stops/hides HostId < 0 which should never happen
  arbitrary = oneof $ map return [minBound..maxBound]

data FileContent
    = FileContent String
    | FileEmpty
  deriving (Eq, Show)

instance Arbitrary FileContent where
  arbitrary = frequency [
    (1, return FileEmpty),
    (9, oneof $ map (return . FileContent) $ words (
        "hello world I just need enough words to make sure \
        \that there are many different file contents generated \
        \thus creating conflicts between updates"))]

data Operation
    = OpRead HostId
    | OpWrite HostId FileContent
    | OpSleep Time
    | OpStabilize
  deriving (Show)

data Scenario = Scenario { scenarioMaxHost :: HostId, scenarioOps :: [Operation] }
  deriving (Show)

-- | Auxiliary function computing the list of hosts (clients)
-- participating in the test scenario
hostList :: HostId -> [HostId]
hostList maxHost = [minBound .. maxHost]

instance Arbitrary Scenario where
  -- after shrinking, last element must still be OpStabilize
  shrink (Scenario { scenarioOps=[] }) = []
  shrink s@(Scenario { scenarioOps=[_], .. }) = [] -- if only one elt left (assumed to be OpStabilize) there is nothing to shrink
  shrink _ = undefined
  arbitrary = do
    -- We always want at least 2 hosts to participate in the
    -- experiment, hence the `succ':
    h <- oneof $ map return [succ minBound .. maxBound]
    f <- oneof [arbSleep, arbWrite]
    completeScenario h f []
    where
      arbHost :: HostId -> Gen HostId
      arbHost h = oneof $ map return [H1 .. h]
      arbRead :: Gen Operation
      arbRead = OpRead <$> arbitrary
      arbWrite :: Gen Operation
      arbWrite = OpWrite <$> arbitrary <*> arbitrary
      arbSleep :: Gen Operation
      arbSleep = OpSleep <$> arbitrary
      arbStabilize :: Gen Operation
      arbStabilize = return OpStabilize
      completeScenario :: HostId -> Operation -> [Operation] -> Gen Scenario
      completeScenario maxHost last revList = do
        let (frRead, frWrite, frSleep, frStabilize) = case last of
                                                        OpRead _ -> (1, 2, 1, 2)
                                                        OpWrite _ _ -> (1, 2, 2, 1)
                                                        OpSleep _ -> (2, 2, 1, 2)
                                                        OpStabilize -> (3, 4, 1, 0)
        endHere <- frequency [(3, return False), (1, return True)]
        case last of
          OpStabilize | endHere -> return $ Scenario {
                          scenarioMaxHost=maxHost
                          , scenarioOps = reverse $ last:revList }
          _ -> do
            next <- frequency [(frRead, arbRead),
                               (frWrite, arbWrite),
                               (frSleep, arbSleep),
                               (frStabilize, arbStabilize)]
            completeScenario maxHost next $ last:revList

type Conflicts = [(HostId, FileContent)]

data StabilizationResult
    = StabOK FileContent Conflicts
    | StabFail [(HostId, FileContent, Conflicts)]

class HasConfig c where
  -- | Extract function from configuration to map a client to its @FilePath@
  -- under test
  filePathUnderTest :: c -> HostId -> FilePath
  -- | Extract function from configuration to map a client to its 2 status
  -- files used to timestamp when the synchronisation started and ended.
  clientStatusUnderTest :: c -> HostId -> (FilePath, FilePath)

data SimplisticConfig = SimplisticConfig
  { scRoot :: FilePath
  , scBasename :: FilePath
  }

_scHostPath :: SimplisticConfig -> HostId -> FilePath
_scHostPath (SimplisticConfig {..}) hostId = scRoot </> ('h':(show $ fromEnum hostId))

instance HasConfig SimplisticConfig where
  filePathUnderTest sc@(SimplisticConfig {..}) h =
    _scHostPath sc h </> scBasename
  clientStatusUnderTest sc@(SimplisticConfig {..}) h =
    let f s = let root = _scHostPath sc h
              in takeDirectory root </> (s ++ takeFileName root)
    in (f ".start.", f ".stop.")

minCfg = SimplisticConfig
  { scRoot = "/tmp"
  , scBasename = "testFile"
  }

-- | The name of the directory holding a client's files is also used
-- as the client's name to disambiguate conflict file names.
clientName :: HasConfig c
  => c -- ^ configuration
  -> HostId -- ^ which client's name is required
  -> FilePath -- ^ client's name
clientName c hostId =
  takeFileName $ takeFileName $ filePathUnderTest c hostId

class (Monad m) => SynctestIO m where
  doRead :: HasConfig c => HostId -> ReaderT c m FileContent
  doWrite :: HasConfig c => HostId -> FileContent -> ReaderT c m FileContent
  doSleep :: Time -> ReaderT c m ()
  doStabilize :: HasConfig c => HostId -> ReaderT c m StabilizationResult

data Observation
    = ObsRead HostId FileContent
    | ObsWrite HostId FileContent
    | ObsSleep 
    | ObsStabilize FileContent Conflicts
    | ObsFailedStabilize [(HostId, FileContent, Conflicts)]
  deriving (Show)

performOperation :: (HasConfig c, SynctestIO m)
  => HostId -- ^ hosts in in range minBound .. maxHost participate in test
  -> Operation
  -> ReaderT c m Observation
performOperation _ (OpRead hId) = do
  c <- doRead hId
  return $ ObsRead hId c
performOperation _ (OpWrite hId fc) = do
  oldContent <- doRead hId
  doWrite hId fc
  return $ ObsWrite hId oldContent
performOperation _(OpSleep time) = doSleep time >> return ObsSleep
performOperation maxHost OpStabilize = do
  stab <- doStabilize maxHost
  return $ case stab of
             StabOK fc cs -> ObsStabilize fc cs
             StabFail csMap -> ObsFailedStabilize csMap

performScenario :: (HasConfig c, SynctestIO m) => Scenario -> ReaderT c m [Observation]
performScenario (Scenario { scenarioMaxHost=maxHost, scenarioOps=scenarioOps })
  = sequence $ map (performOperation maxHost) scenarioOps

askRelativeToHost :: (HasConfig c, Monad m) => (c -> HostId -> o) -> HostId -> ReaderT c m o
askRelativeToHost f hostId = do
  fpFun <- asks f
  return $ fpFun hostId

askFilePathUnderTest :: (HasConfig c, Monad m) => HostId -> ReaderT c m FilePath
askFilePathUnderTest = askRelativeToHost filePathUnderTest

askClientStatusUnderTest :: (HasConfig c, Monad m) => HostId -> ReaderT c m (FilePath, FilePath)
askClientStatusUnderTest = askRelativeToHost clientStatusUnderTest

askConflictFile :: (HasConfig c, Monad m)
  => HostId -- ^ Which client contains the conflict file
  -> HostId -- ^ Whose client data is contained in the conflict file
  -> ReaderT c m FilePath
askConflictFile host conflict = do
  root <- askFilePathUnderTest host
  cnflct <- askRelativeToHost clientName conflict
  return $ root ++ "." ++ cnflct
  
lowLevelReadIO :: (MonadIO m, HasConfig c) => HostId -> ReaderT c m (FilePath, FileContent)
lowLevelReadIO hostId = do
  fp <- askFilePathUnderTest hostId
  c' <- liftIO $ tryIOError $ readFile fp
  return $ (fp
           , case c' of
               Left _ -> FileEmpty
               Right c -> if length c == 0 then FileEmpty else FileContent c)
  
doReadIO :: (MonadIO m, HasConfig c) => HostId -> ReaderT c m FileContent
doReadIO = fmap snd . lowLevelReadIO
  
doWriteIO :: (MonadIO m, HasConfig c) => HostId -> FileContent -> ReaderT c m FileContent
doWriteIO hostId fc = do
  (fp, oldContent) <- lowLevelReadIO hostId
  case (oldContent, fc) of
    (FileEmpty, FileEmpty) -> return FileEmpty
    _ -> do
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

shiftUTCTime :: UTCTime -> DiffTime -> UTCTime
shiftUTCTime t@(UTCTime { utctDayTime=u }) offs = t { utctDayTime=u + offs }

-- | How many seconds to wait maximum for stabilization to occur
stabilizeTimeout = 120 :: DiffTime

-- | When logfile of unisonsync.sh call isn't updated/created
-- anymore, the syncing sees no new data to transfer (since
-- 05bf67e5a1227a0bf220bc972f139f16900d08f9).  Detect this with a
-- timeout...
--
-- As long as a conflict remains, the logfile will be updated with the
-- information of that conflict.  This will cause a timeout.
doStabilizeIO :: (MonadIO m, HasConfig c)
  => HostId -- ^ to know how many hosts there are
  -> ReaderT c m StabilizationResult
doStabilizeIO maxHost = do
  startTime <- liftIO $ getCurrentTime
  let stopTime = startTime `shiftUTCTime` stabilizeTimeout
  mbSyncedOnce <- waitAllClientsSyncedAtLeastOnce maxHost startTime stopTime
  case mbSyncedOnce of
    -- TODO: decide how to make this function total
    Nothing -> error "doStabilizeIO: failed to run all syncs before timeout (1st time)"
    Just firstSyncTime -> do
      mbSyncedTwice <- waitAllClientsSyncedAtLeastOnce maxHost firstSyncTime stopTime
      case mbSyncedTwice of
        -- TODO: decide how to make this function total
        Nothing -> error "doStabilizeIO: failed to run all syncs before timeout (2nd time)"
        Just _ -> gatherStabilizationResultsIO maxHost

-- | Return list of conflict files found on a client
doReadConflicts :: (MonadIO m, HasConfig c)
  => HostId -- ^ hosts under test are [(minBound :: HostId)..maxHost]
  -> HostId -- ^ which client to query
  -> ReaderT c m Conflicts
doReadConflicts maxHost hostId = do
  fput <- askFilePathUnderTest hostId
  let dir = takeDirectory fput
  let filename = takeFileName fput
  let clientList = hostList maxHost
  -- Read conflict files found on client `hostId' into a list of
  -- [Maybe (HostId, FileContent)] ...
  tmp <- flip mapM clientList $ \conflictHostId -> do
    cF <- askConflictFile hostId conflictHostId
    exists <- liftIO $ doesFileExist cF
    if exists
    then do
      -- TODO: I didn't wrap this in tryIOError because I haven't
      -- decided how to handle errors
      fc' <- liftIO $ readFile cF
      let fc = case fc' of
                 "" -> FileEmpty
                 _ -> FileContent fc'
      return $ Just (conflictHostId, fc)
    else return Nothing
  -- ... filtered to only keep the `Just _'
  return $ foldr (\elt acc -> maybe acc (:acc) elt) [] tmp

-- | Read file under test and conflict copies on all clients.
--
-- If each client has the same content and conflict copies, the
-- stabilization was a success, otherwise, it was a failure.
--
-- NB: testing with an empty list of clients makes no sense and should
-- never happen but to keep the function total, 'StabFail' is returned
-- instead.
gatherStabilizationResultsIO :: (MonadIO m, HasConfig c)
  => HostId -- ^ highest host number, hosts under test are [(minBound :: HostId)..maxHost]
  -> ReaderT c m StabilizationResult
gatherStabilizationResultsIO maxHost = do
  let clientList = hostList maxHost
  contents <- mapM doReadIO clientList
  conflicts <- mapM (doReadConflicts maxHost) clientList
  case zip contents conflicts of
    [] -> return $ StabFail []
    ref@(cntnt, cnflct):cts -> return $
      if all (== ref) cts
      then StabOK cntnt cnflct
      else StabFail $ zip3 clientList contents conflicts

-- | 'True' if a synchronization ran for the client after the given time
clientSyncedAfter :: (MonadIO m, HasConfig c)
  => UTCTime -- ^ Only accept synchronisation runs happening after this time
  -> HostId  -- ^ Which client is the focus
  -> ReaderT c m Bool -- ^ @True@ if the client contacted the server after
                      -- @startTime@
clientSyncedAfter startTime hostId = do
  (syncStart, syncStop) <- askClientStatusUnderTest hostId
  etStartTime <- liftIO $ tryIOError $ getModificationTime syncStart
  etStopTime <- liftIO $ tryIOError $ getModificationTime syncStop
  return $ compareTime etStartTime etStopTime
  where compareTime (Left _) _ = False
        compareTime _ (Left _) = False
        compareTime (Right syncStartTime) (Right syncStopTime) =
          (startTime < syncStartTime) && (syncStartTime <= syncStopTime)

-- | Wait with timeout until all clients have contacted the server at least
-- once.  This doesn't necessarily mean that synchronisation was successful.
waitAllClientsSyncedAtLeastOnce :: (MonadIO m, HasConfig c)
  => HostId -- ^ clients are [(minBound :: HostId)..maxHost]
  -> UTCTime -- ^ Only accept synchronisation runs happening after this time
             -- (see 'clientSyncedAfter')
  -> UTCTime -- ^ Timeout
  -> ReaderT c m (Maybe UTCTime) -- ^ @Nothing@ for timeout or @Just t@ for
                                 -- timestamp when all clients had
                                 -- synchronised at least once.
waitAllClientsSyncedAtLeastOnce maxHost startTime deadline = do
  now <- liftIO $ getCurrentTime
  if now >= deadline
    then return Nothing
    else do
      let clientList = hostList maxHost
      clientStatus <- fmap and
                         $ mapM (clientSyncedAfter startTime)
                                (clientList :: [HostId])
      if clientStatus
        then return $ Just now
        else do
          liftIO $ threadDelay 1000 -- ms
          waitAllClientsSyncedAtLeastOnce maxHost startTime deadline

-- (Monad m, MonadIO m) => SynctestIO m would require
-- {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- and I'm not sure it's worth it.
instance SynctestIO IO where
  doRead = doReadIO
  doWrite = doWriteIO
  doSleep = doSleepIO
  doStabilize = doStabilizeIO
  
