-- Was example from https://github.com/chpatrick/haskell-hidapi/
--
-- Open and dump the first HID device. Should print deltas (?) for a USB mouse.
-- You need root or a udev rule for your device.
-- https://github.com/signal11/hidapi/blob/master/udev/99-hid.rules
-- This seems to claim the device until it is unplugged.
--
-- I adapted it to look for a specific device (optical mouse)

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Char (chr)
import Data.Int
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

pp :: DeviceInfo -> String
pp di = "di:path=" ++ show (path di)
     ++ "\n   vendorId=" ++ showHex (vendorId di)
     ++ "\n   productId=" ++ showHex (productId di)
     ++ "\n   serialNumber=" ++ show (serialNumber di)
     ++ "\n   releaseNumber=" ++ show (releaseNumber di)
     ++ "\n   manufacturerString=" ++ show (manufacturerString di)
     ++ "\n   productString=" ++ show (productString di)
     ++ "\n   usagePage=" ++ show (usagePage di)
     ++ "\n   usage=" ++ show (usage di)
     ++ "\n   interfaceNumber=" ++ show (interfaceNumber di)

main :: IO ()
main = withHIDAPI $ do
  dis <- enumerateAll
  forM_ dis $ \di -> do
    putStrLn $ "\n----\n" ++ pp di
    case (vendorId di, interfaceNumber di) of
      (0x15ca, _) -> do -- Optical mouse
        d <- openDeviceInfo di
        forM_ (replicate 10 ()) $ \_ -> do
          bs <- HID.read d 6
          putStr (show (fromIntegral (BS.index bs 1) :: Int8))
          putChar ' '
          print (fromIntegral (BS.index bs 2) :: Int8)
        close d
      (0x1d50, 1) -> do -- OnlyKey
        d <- openDeviceInfo di
        r <- HID.write d $ BS.pack $ take 64 $ [255, 255, 255, 255, 229] ++ repeat 0
        putStrLn $ "Write returned 0x" ++ showHex r
        threadDelay 500000
        bs <- HID.read d 10
        _ <- forM [0..BS.length bs - 1] $ \i -> do
               putStr $ ascii $ BS.index bs i
               putChar ' '
        putStrLn "..."
        close d
      _ -> putStrLn "NOP"
