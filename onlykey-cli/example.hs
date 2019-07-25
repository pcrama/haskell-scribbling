{-# LANGUAGE OverloadedStrings #-}

-- Was example from https://github.com/chpatrick/haskell-hidapi/
--
-- Open and dump the first HID device. Should print deltas (?) for a USB mouse.
-- You need root or a udev rule for your device.
-- https://github.com/signal11/hidapi/blob/master/udev/99-hid.rules
-- This seems to claim the device until it is unplugged.
--
-- I adapted it to look for a specific device (optical mouse)

import Control.Concurrent (mkWeakMVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (mask_)
import Control.Monad
import Data.Functor (($>))
import Data.List (find, intercalate)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import Data.Vector as V ((!), toList)
import qualified System.HIDAPI as HID
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.USB

main :: IO ()
main = do
  args <- getArgs
  let mbIds =
        case args of
          [] -> Just (0x1d50, 0x60fc) -- default vendorId & productId of onlykey
          [vendorIdStr, productIdStr] -> Just (read vendorIdStr, read productIdStr)
          _ -> Nothing
  case mbIds of
    Nothing -> fail "two arguments or nothing"
    Just (vendorId, productId) -> doMain vendorId productId

doMain :: HID.VendorID -> HID.ProductID -> IO ()
doMain vendorId productId = do
  -- Initialization:
  ctx <- newCtx
  setDebug ctx PrintDebug

  -- Device retrieval:
  dev <- if ctx `hasCapability` HasHotplug
         then waitForMyDevice ctx vendorId productId
         else findMyDevice ctx vendorId productId

  -- Device usage:
  doSomethingWithDevice dev

waitForMyDevice :: Ctx -> HID.VendorID -> HID.ProductID -> IO Device
waitForMyDevice ctx vendorId productId = do
  putStrLn "Waiting for device attachment..."
  mv <- newEmptyMVar
  mask_ $ do
    h <- registerHotplugCallback ctx
                                 deviceArrived
                                 enumerate
                                 (Just vendorId)
                                 (Just productId)
                                 Nothing
                                 (\dev event ->
                                    tryPutMVar mv (dev, event) $>
                                      DeregisterThisCallback)
    void $ mkWeakMVar mv $ deregisterHotplugCallback h
  (dev, _event) <- takeMVar mv
  return dev

-- Enumerate all devices and find the right one.
findMyDevice :: Ctx -> HID.VendorID -> HID.ProductID -> IO Device
findMyDevice ctx vendorId productId = do
    devs <- toList <$> getDevices ctx
    deviceDescs <- mapM getDeviceDesc devs
    case fmap fst $ find (match . snd) $ zip devs deviceDescs of
      Nothing  -> hPutStrLn stderr "Mouse not found" >> exitFailure
      Just dev -> return dev
  where
    match :: DeviceDesc -> Bool
    match devDesc =  deviceVendorId  devDesc == vendorId
                  && deviceProductId devDesc == productId

doSomethingWithDevice :: Device -> IO ()
doSomethingWithDevice dev = do
  putStrLn $ unlines $ deviceInfo dev

  putStrLn "Opening device..."
  withDeviceHandle dev $ \devHndl -> do

    putStrLn "Detaching kernel driver..."
    withDetachedKernelDriver devHndl 0 $ do

      putStrLn "Claiming interface..."
      withClaimedInterface devHndl 0 $ do

        -- Inspecting descriptors:
        config0 <- getConfigDesc dev 0
        let interface0 = configInterfaces config0 ! 0
            alternate0 = interface0 ! 0
            endpoint1  = interfaceEndpoints alternate0 ! 0
            mps        = maxPacketSize $ endpointMaxPacketSize endpoint1
            timeout    = 5000

        printf "maxPacketSize = %i\n" mps

        putStrLn "Creating transfer..."
        readTrans <- newReadTransfer
                       InterruptTransfer
                       devHndl
                       (endpointAddress endpoint1)
                       0
                       timeout

        -- Performing I/O:
        let n = 3 :: Int
        forM_ [0..n-1] $ \i -> do
          let size = (2^i) * mps

          _ <- printf "(%i/%i) reading %i bytes during a maximum of %i ms...\n"
                      (i+1) n size timeout

          setReadTransferSize readTrans size

          (bs, status) <- performReadTransfer readTrans

          when (status == TimedOut) $ putStrLn "Reading timed out!"
          _ <- printf "Read %i bytes:\n" $ BS.length bs
          printBytes bs

deviceInfo :: Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ deviceSpeed dev)
  , printf "busNumber:     %s" (show $ busNumber dev)
  , printf "portNumber:    %s" (show $ portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . toList) $
                                  portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ deviceAddress dev)
  ]

printBytes :: BS.ByteString -> IO ()
printBytes = putStrLn . intercalate " " . map (printf "0x%02x") . BS.unpack

-- requestLabels :: WriteExactAction
-- requestLabels bs timeoutMs = do
