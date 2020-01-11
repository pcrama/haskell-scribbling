-- Was example from https://github.com/chpatrick/haskell-hidapi/
--
-- Should print deltas (?) for a USB mouse.
-- You need root or a udev rule for your device.
-- https://github.com/signal11/hidapi/blob/master/udev/99-hid.rules
-- This seems to claim the device until it is unplugged.
--
-- I adapted it to look for a specific device (optical mouse or OnlyKey)

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Char (chr)
import Data.Int
import Data.List (intercalate)
import qualified Data.ByteString as BS
import System.HIDAPI as HID

showHex :: Integral a => a -> String
showHex = go ""
  where go s x | x < 16 = s ++ (hexChar x:"")
               | otherwise = let hd = x `div` 16
                                 tl = x `mod` 16
                             in go s hd ++ (hexChar tl:"")
        hexChar = ("0123456789abcdef" !!) . fromIntegral

ascii :: Integral a => a -> String
ascii x | 32 <= x && x < 127 = (chr $ fromIntegral x):""
        | otherwise = "<" ++ showHex x ++ ">"

asciiString :: BS.ByteString -> String
asciiString = intercalate " " . map ascii . BS.unpack

pp :: DeviceInfo -> String
pp di = "di:path=" ++ show (path di)
     ++ "\n   vendorId=" ++ showHex (vendorId di)
     ++ "\n   productId=" ++ showHex (productId di)
     ++ "\n   serialNumber=" ++ show (serialNumber di)
     ++ "\n   releaseNumber=" ++ show (releaseNumber di)
     ++ "\n   manufacturerString=" ++ show (manufacturerString di)
     ++ "\n   productString=" ++ show (productString di)
     ++ "\n   usagePage=" ++ showHex (usagePage di)
     ++ "\n   usage=" ++ show (usage di)
     ++ "\n   interfaceNumber=" ++ show (interfaceNumber di)


useOnlyKey :: DeviceInfo -> IO ()
useOnlyKey di = do -- OnlyKey
    d <- openDeviceInfo di
    r <- HID.write d $ BS.pack $ take 32 $ [255, 255, 255, 255, 229] ++ repeat 0
    putStrLn $ "Write returned 0x" ++ showHex r
    threadDelay 500000
    putStrLn "Reading..."
    _ <- sequence $ replicate 12 $ do
      bs <- HID.read d 16
      putStrLn $ asciiString bs
    putStrLn " ... Done"
    putStrLn "..."
    close d

-- | Detect correct DeviceInfo for OnlyKey
-- The OnlyKey yields several DeviceInfo matching the desired vendor &
-- product ID, yet only one of them can be used for
-- communicating.  Selecting it is based on the following Python code:
--
--              # https://github.com/trustcrypto/python-onlykey/blob/946f9ab657a14dad5dfb09f2d00d2d67ac38a78d/onlykey/client.py
--              # lines 181-189
--              if (vendor_id, product_id) in DEVICE_IDS:
--                  if serial_number == '1000000000':
--                      if usage_page == 0xffab or interface_number == 2:
--                          self._hid.open_path(path)
--                          self._hid.set_nonblocking(True)
--                  else:
--                      if usage_page == 0xf1d0 or interface_number == 1:
--                          self._hid.open_path(path)
--                          self._hid.set_nonblocking(True)
checkUsagePageAndInterfaceNumber :: DeviceInfo -> Bool
checkUsagePageAndInterfaceNumber di
  | serialNumber di == Just "1000000000" = usagePage di == 0xffab || interfaceNumber di == 2
  | otherwise                            = usagePage di == 0xf1d0 || interfaceNumber di == 1

main :: IO ()
main = withHIDAPI $ do
  dis <- enumerateAll
  forM_ dis $ \di -> do
    putStrLn $ "\n----\n" ++ pp di
    case (vendorId di, productId di) of
      (0x15ca, _) -> do -- Optical mouse
        d <- openDeviceInfo di
        forM_ (replicate 10 ()) $ \_ -> do
          bs <- HID.read d 6
          putStr (show (fromIntegral (BS.index bs 1) :: Int8))
          putChar ' '
          print (fromIntegral (BS.index bs 2) :: Int8)
        close d
      (0x1d50, 0x60fc)
        | checkUsagePageAndInterfaceNumber di -> useOnlyKey di
        | otherwise -> putStrLn "Ignoring (0x1d50, 0x60fc)"
      (0x16c0, 0x0486)
        | checkUsagePageAndInterfaceNumber di -> useOnlyKey di
        | otherwise -> putStrLn "Ignoring (0x16c0, 0x0486)"
      _ -> putStrLn "NOP"
