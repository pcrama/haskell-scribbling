module Utils (
  makeData,
  removeMultiples,
  rleCompress
)

where

import qualified Data.ByteString as B

import Data.Array ((!), Array, bounds, listArray)
import Data.Char (chr)
import Data.Ord (comparing)
import Data.List (unfoldr, sortBy)
import Data.Word (Word8)
import System.Random (RandomGen, mkStdGen, randomR, split)

import Categories

rleCompress :: (Foldable f, Eq a) => f a -> [(a, Int)]
rleCompress = foldr updateCountIfEqualOtherwisePrepend []
  where updateCountIfEqualOtherwisePrepend x [] = [(x, 1)]
        updateCountIfEqualOtherwisePrepend x z@((y, c):xs) | x == y    = (y, c + 1):xs
                                                           | otherwise = (x, 1):z

isMultipleOf :: Eq a => [a] -> [a] -> Bool
isMultipleOf long short = go short long short
  where go _ [] [] = True
        go _ [] (_:_) = False
        go _ (_:_) [] = False
        go _ [x] [y] = x == y
        go init (x:xs) (y:ys) | x == y = go init xs $ if null ys then init else ys
                              | otherwise = False

removeMultiples, _removeMultiples :: Eq a => [[a]] -> [[a]]
removeMultiples = _removeMultiples . sortBy (comparing length)
_removeMultiples [] = []
_removeMultiples [x] = [x]
_removeMultiples (x:xs) = x:[y | y <- xs, not (y `isMultipleOf` x)]

_makeSubData :: RandomGen g => Int -> Array Int a -> g -> [a]
_makeSubData n alph gen = unfoldr prependNewCharOrStop (gen, 0)
  where prependNewCharOrStop (g, lenSoFar)
          | lenSoFar >= n = Nothing
          | otherwise = let (idx, g') = randomR (bounds alph) g
                        in Just (alph ! idx, (g', lenSoFar + 1))

{- Use like this to generate test data
Data.ByteString.writeFile "../testdata/At40_Ad5_100000.dat" $ makeData 1243 [(ASCIIText, 40), (ASCIINum, 5)] 100000
Data.ByteString.writeFile "../testdata/At40_Ad5_10000.dat" $ makeData 567843 [(ASCIIText, 40), (ASCIINum, 5)] 10000
Data.ByteString.writeFile "../testdata/Et40_Ad5_PD3_10000.dat" $ makeData 568743 [(EBCDICText, 40), (ASCIINum, 5), (PackedDecimal, 3)] 10000
Data.ByteString.writeFile "../testdata/Et40_Ad5_PD6_100000.dat" $ makeData 9999 [(EBCDICText, 40), (ASCIINum, 5), (PackedDecimal, 6)] 100000
Data.ByteString.writeFile "../testdata/Et64_Ed8_Bin56_100007.dat" $ makeData 99999 [(EBCDICText, 64), (EBCDICNum, 8), (BLOB, 56)] 100007
Data.ByteString.writeFile "../testdata/Et32_Ed8_Bin23_Ad4_PD8_500123.dat" $ makeData 989898 [(EBCDICText, 32), (EBCDICNum, 8), (BLOB, 23), (ASCIINum, 4), (PackedDecimal, 8)] 500123
Data.ByteString.writeFile "../testdata/Et32_Ed8_At22_Ad4_PD8_500123.dat" $ makeData 765108 [(EBCDICText, 32), (EBCDICNum, 8), (ASCIIText, 22), (ASCIINum, 4), (PackedDecimal, 8)] 500123
-}
makeData :: Int -> [(BroadCategory, Int)] -> Int -> B.ByteString
makeData seed schema len =
    let binaryChars = [0..255 :: Word8]
        binaryArray = listArray (0, 255) binaryChars
        asciiTextChars' = [x | x <- binaryChars
                             , (broadCategory PreferTextual PreferASCII
                                            $ narrowestCategory x)
                               == ASCIIText]
        asciiTextLen = length asciiTextChars'
        asciiTextChars = listArray (0, asciiTextLen - 1) asciiTextChars'
        ebcdicTextChars' = [x | x <- binaryChars
                              , (broadCategory PreferTextual PreferEBCDIC
                                             $ narrowestCategory x)
                                == EBCDICText]
        ebcdicTextLen = length ebcdicTextChars'
        ebcdicTextChars = listArray (0, ebcdicTextLen - 1) ebcdicTextChars'
        asciiDigits' = [x | x <- binaryChars
                          , narrowestCategory x == Ad_At_PD]
        asciiDigitsLen = length asciiDigits'
        asciiDigits = listArray (0, asciiDigitsLen - 1) asciiDigits'
        ebcdicDigits' = [x | x <- binaryChars
                           , narrowestCategory x == Ed_Et]
        ebcdicDigitsLen = length ebcdicDigits'
        ebcdicDigits = listArray (0, ebcdicDigitsLen - 1) ebcdicDigits'
        packedDecimal' = [x | x <- binaryChars
                            , narrowestCategory x `elem` [ PD
                                                         , At_PD
                                                         , Ad_At_PD
                                                         , At_Et_PD
                                                         , Et_PD]]
        packedDecimalLen = length packedDecimal'
        packedDecimal = listArray (0, packedDecimalLen - 1) packedDecimal'
        paddedDecimal' = [x | x <- binaryChars
                            , narrowestCategory x `elem` [ PP
                                                         , At_PP   
                                                         , At_Et_PP
                                                         , Et_PP]]
        paddedDecimalLen = length paddedDecimal'
        paddedDecimal = listArray (0, paddedDecimalLen - 1) paddedDecimal'
        generateNewCharOrStop (g, s, buf, soFar)
          | soFar >= len = Nothing
          | null buf = let ((cat, count):ss) = s
                           (g', nextBufG) = split g
                           (newChar:nextBuf) = _makeSubData count
                                                            (case cat of
                                                               BLOB -> binaryArray
                                                               ASCIIText -> asciiTextChars
                                                               ASCIINum -> asciiDigits
                                                               EBCDICText -> ebcdicTextChars
                                                               EBCDICNum -> ebcdicDigits
                                                               -- TODO: padded decimal?
                                                               PackedDecimal -> packedDecimal)
                                                            nextBufG
                       in Just (newChar, ( g'
                                         , if null ss then schema else ss
                                         , nextBuf
                                         , soFar + 1))
          | otherwise = Just (head buf, (g, s, tail buf, soFar + 1))
    in B.unfoldr generateNewCharOrStop (mkStdGen seed, schema, [], 0)
