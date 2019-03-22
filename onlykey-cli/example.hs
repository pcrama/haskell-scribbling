-- Was example from https://github.com/chpatrick/haskell-hidapi/
--
-- Open and dump the first HID device. Should print deltas (?) for a USB mouse.
-- You need root or a udev rule for your device.
-- https://github.com/signal11/hidapi/blob/master/udev/99-hid.rules
-- This seems to claim the device until it is unplugged.
--
-- I adapted it to look for a specific device (optical mouse)

import Control.Monad
import Data.Int
import qualified Data.ByteString as BS
import System.HIDAPI as HID

showHex :: (Integral a) => a -> String
showHex = go ""
  where go s x | x < 16 = s ++ (hexChar x:"")
               | otherwise = let hd = x `div` 16
                                 tl = x `mod` 16
                             in go s hd ++ (hexChar tl:"")
        hexChar = ("0123456789abcdef" !!) . fromIntegral

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
    case vendorId di of
      0x15ca -> do
           d <- openDeviceInfo di
           forever $ do
             bs <- HID.read d 6
             putStr (show (fromIntegral (BS.index bs 1) :: Int8))
             putChar ' '
             print (fromIntegral (BS.index bs 2) :: Int8)
      _ -> putStrLn (pp di)
