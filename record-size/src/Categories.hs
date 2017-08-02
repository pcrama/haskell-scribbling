module Categories (
  BroadCategory(..),
  NarrowestCategory(..),
  NumTextPref(..),
  SystemPref(..),
  broadCategory,
  byteStringOps,
  canBeNumeric,
  canBeTextual,
  loadFile,
  narrowestCategory,
  stringOps,
  toNarrowCategoriesArray,
  widen
)

where

import Data.Array ((!), Array, bounds, elems, listArray)
import Data.Bits
import qualified Data.ByteString as B
import Data.Char (ord, chr)
import Data.List (nub, intercalate)
import Data.Word (Word8)

type CP = Char

class CodePoint c where
  isASCIItext, isEBCDICtext, isASCIIdigit, isEBCDICdigit, isPackedDecimalNoPadding, isPackedDecimalPadded :: c -> Bool

instance CodePoint Char where
  isASCIItext x = (' ' <= x) && (x <= '\x7f')
  isEBCDICtext x = ('\x40' <= x) && (x < '\xff') -- https://en.wikipedia.org/wiki/EBCDIC says "... Characters 00–3F and FF are controls ..."
  isASCIIdigit x = ('0' <= x) && (x <= '9')
  isEBCDICdigit x = ('\xf0' <= x) && (x <= '\xf9')
  -- Never heard about packed decimal (I only know binary coded decimal),
  -- but given Raincode lab's Cobol ties, I hope that this definition is OK:
  -- http://simotime.com/datapk01.htm
  isPackedDecimalNoPadding x = let hi = ord x `shiftR` 4
                                   lo = ord x .&. 0xf
                               in ((0 <= hi) && (hi <= 9)
                                && (0 <= lo) && (lo <= 9))
  isPackedDecimalPadded x = let hi = ord x `shiftR` 4
                                lo = ord x .&. 0xf
                            in ((0 <= hi) && (hi <= 9)
                             && (lo == 12) || (lo == 13) || (lo == 14))

instance CodePoint Word8 where
  isASCIItext x = (0x20 <= x) && (x <= 0x7f)
  isEBCDICtext x = (0x40 <= x) && (x < 0xff) -- https://en.wikipedia.org/wiki/EBCDIC says "... Characters 00–3F and FF are controls ..."
  isASCIIdigit x = (0x30 <= x) && (x <= 0x39)
  isEBCDICdigit x = (0xf0 <= x) && (x <= 0xf9)
  -- Never heard about packed decimal (I only know binary coded decimal),
  -- but given Raincode lab's Cobol ties, I hope that this definition is OK:
  -- http://simotime.com/datapk01.htm
  isPackedDecimalNoPadding x = let hi = x `shiftR` 4
                                   lo = x .&. 0xf
                               in ((0 <= hi) && (hi <= 9)
                                && (0 <= lo) && (lo <= 9))
  isPackedDecimalPadded x = let hi = x `shiftR` 4
                                lo = x .&. 0xf
                            in ((0 <= hi) && (hi <= 9)
                             && (lo == 12) || (lo == 13) || (lo == 14))

-- Used to generate the constructors of CodePointState with
-- nub $ map (generateIsXXXInfo . chr) [0..255]
generateIsXXXInfo :: CodePoint cp => cp -> String
generateIsXXXInfo x = mapEmptyToBinary
                    $ intercalate "_"
                    $ filter (/= "")
                    -- alphabetic ordering of strings is important for Elisp code
                    $ map ($ x) [desc "Ad" isASCIIdigit
                               , desc "At" isASCIItext
                               , desc "Ed" isEBCDICdigit
                               , desc "Et" isEBCDICtext
                               , desc "PD" isPackedDecimalNoPadding
                               , desc "PP" isPackedDecimalPadded
                               ]
  where desc s f x | f x = s
                   | otherwise = ""
        mapEmptyToBinary "" = "Binary"
        mapEmptyToBinary x = x

-- Most precise category a codepoint belongs to, e.g. Ad_At_PD is an ASCII digit
-- (automatically implies ASCII text) and a packed decimal, but PD is a packed
-- decimal (no padding) outside the ASCII or EBCDIC text range
data NarrowestCategory = PD
                       | Binary
                       | PP
                       | At_PD
                       | At
                       | At_PP
                       | Ad_At_PD
                       | At_Et_PD
                       | At_Et
                       | At_Et_PP
                       | Et_PD
                       | Et
                       | Et_PP
                       | Ed_Et
  deriving (Eq, Show)

canBeNumeric PD       = True
canBeNumeric Binary   = False
canBeNumeric PP       = True
canBeNumeric At_PD    = True
canBeNumeric At       = False
canBeNumeric At_PP    = True
canBeNumeric Ad_At_PD = True
canBeNumeric At_Et_PD = True
canBeNumeric At_Et    = False
canBeNumeric At_Et_PP = True
canBeNumeric Et_PD    = True
canBeNumeric Et       = False
canBeNumeric Et_PP    = True
canBeNumeric Ed_Et    = True

canBeTextual PD       = False
canBeTextual Binary   = False
canBeTextual PP       = False
canBeTextual At_PD    = True
canBeTextual At       = True
canBeTextual At_PP    = True
canBeTextual Ad_At_PD = True
canBeTextual At_Et_PD = True
canBeTextual At_Et    = True
canBeTextual At_Et_PP = True
canBeTextual Et_PD    = True
canBeTextual Et       = True
canBeTextual Et_PP    = True
canBeTextual Ed_Et    = True

-- Test this with (should return empty list)
-- let all = map chr [0..255] ; dup f g x = (f x, g x) in filter (uncurry (/=)) $ map (dup generateIsXXXInfo $ show . narrowestCategory) all
narrowestCategory :: CodePoint c => c -> NarrowestCategory
narrowestCategory x
  | not $    isASCIItext x || isASCIIdigit x
          || isEBCDICtext x || isEBCDICdigit x
          || isPackedDecimalNoPadding x || isPackedDecimalPadded x
  = Binary
  |  isASCIIdigit x && isASCIItext x && isPackedDecimalNoPadding x
  && not (isEBCDICtext x || isEBCDICdigit x || isPackedDecimalPadded x)
  = Ad_At_PD
  |  isASCIItext x && isEBCDICtext x && isPackedDecimalNoPadding x
  && not (isASCIIdigit x || isEBCDICdigit x || isPackedDecimalPadded x)
  = At_Et_PD
  |  isASCIItext x && isEBCDICtext x && isPackedDecimalPadded x
  && not (isASCIIdigit x || isEBCDICdigit x || isPackedDecimalNoPadding x)
  = At_Et_PP
  |  isASCIItext x && isPackedDecimalNoPadding x
  && not (isASCIIdigit x || isEBCDICdigit x || isEBCDICtext x || isPackedDecimalPadded x)
  = At_PD
  |  isASCIItext x && isPackedDecimalPadded x
  && not (isASCIIdigit x || isEBCDICdigit x || isEBCDICtext x || isPackedDecimalNoPadding x)
  = At_PP
  |  isASCIItext x && isEBCDICtext x
  && not (isASCIIdigit x || isEBCDICdigit x || isPackedDecimalPadded x || isPackedDecimalNoPadding x)
  = At_Et
  |  isEBCDICtext x && isPackedDecimalNoPadding x
  && not (isASCIIdigit x || isASCIItext x || isEBCDICdigit x || isPackedDecimalPadded x)
  = Et_PD
  |  isEBCDICtext x && isPackedDecimalPadded x
  && not (isASCIIdigit x || isASCIItext x || isEBCDICdigit x || isPackedDecimalNoPadding x)
  = Et_PP
  |  isEBCDICdigit x && isEBCDICtext x
  && not (isASCIIdigit x || isASCIItext x || isPackedDecimalPadded x || isPackedDecimalNoPadding x)
  = Ed_Et
  |  isPackedDecimalPadded x
  && not (   isASCIIdigit x || isASCIItext x
          || isEBCDICdigit x || isEBCDICtext x
          || isPackedDecimalNoPadding x)
  = PP
  |  isPackedDecimalNoPadding x
  && not (   isASCIIdigit x || isASCIItext x
          || isEBCDICdigit x || isEBCDICtext x
          || isPackedDecimalPadded x)
  = PD
  |  isASCIItext x
  && not (   isASCIIdigit x
          || isEBCDICdigit x || isEBCDICtext x
          || isPackedDecimalPadded x || isPackedDecimalNoPadding x)
  = At
  |  isEBCDICtext x
  && not (   isASCIIdigit x || isASCIItext x
          || isEBCDICdigit x
          || isPackedDecimalPadded x || isPackedDecimalNoPadding x)
  = Et
  | otherwise
  = error "Should never happen"

-- Return narrowest category that still describes the two inputs
widen :: NarrowestCategory -> NarrowestCategory -> NarrowestCategory
-- Generated by
{- (let ((binary "Binary")
         (possibilities (cl-remove (split-string "PD
                       | Binary
                       | PP
                       | At_PD
                       | At
                       | At_PP
                       | Ad_At_PD
                       | At_Et_PD
                       | At_Et
                       | At_Et_PP
                       | Et_PD
                       | Et
                       | Et_PP
                       | Ed_Et"
                                   "\n *| *")
                                   :test #'string-equal)))
  (insert "widen _ " binary " = " binary "\n"
          "widen " binary " _ = " binary "\n")
  (let ((left-tail possibilities)
        left)                           ; manual macroexpansion of dolist
    (while left-tail                    ; because of strange Emacs error.
      (setq left (car left-tail))       ; I don't have time to debug this
      (setq left-tail (cdr left-tail))  ; so I work around it.  This
      (dolist (right possibilities)     ; dolist is similar but works, why?
        (let* ((left-parts (split-string left "_"))
               (right-parts (split-string right "_"))
               (all (mapconcat
                     (lambda (x) x)
                     (sort (cl-intersection left-parts right-parts :test #'string-equal)
                           #'string-lessp)
                     "_"))
               (result (if (cl-find all possibilities :test #'string-equal)
                           all
                         binary)))
          (insert "widen " left " " right " = " result "\n")))))) -}
widen _ Binary = Binary 
widen Binary _ = Binary
widen PD PD = PD
widen PD PP = Binary
widen PD At_PD = PD
widen PD At = Binary
widen PD At_PP = Binary
widen PD Ad_At_PD = PD
widen PD At_Et_PD = PD
widen PD At_Et = Binary
widen PD At_Et_PP = Binary
widen PD Et_PD = PD
widen PD Et = Binary
widen PD Et_PP = Binary
widen PD Ed_Et = Binary
widen PP PD = Binary
widen PP PP = PP
widen PP At_PD = Binary
widen PP At = Binary
widen PP At_PP = PP
widen PP Ad_At_PD = Binary
widen PP At_Et_PD = Binary
widen PP At_Et = Binary
widen PP At_Et_PP = PP
widen PP Et_PD = Binary
widen PP Et = Binary
widen PP Et_PP = PP
widen PP Ed_Et = Binary
widen At_PD PD = PD
widen At_PD PP = Binary
widen At_PD At_PD = At_PD
widen At_PD At = At
widen At_PD At_PP = At
widen At_PD Ad_At_PD = At_PD
widen At_PD At_Et_PD = At_PD
widen At_PD At_Et = At
widen At_PD At_Et_PP = At
widen At_PD Et_PD = PD
widen At_PD Et = Binary
widen At_PD Et_PP = Binary
widen At_PD Ed_Et = Binary
widen At PD = Binary
widen At PP = Binary
widen At At_PD = At
widen At At = At
widen At At_PP = At
widen At Ad_At_PD = At
widen At At_Et_PD = At
widen At At_Et = At
widen At At_Et_PP = At
widen At Et_PD = Binary
widen At Et = Binary
widen At Et_PP = Binary
widen At Ed_Et = Binary
widen At_PP PD = Binary
widen At_PP PP = PP
widen At_PP At_PD = At
widen At_PP At = At
widen At_PP At_PP = At_PP
widen At_PP Ad_At_PD = At
widen At_PP At_Et_PD = At
widen At_PP At_Et = At
widen At_PP At_Et_PP = At_PP
widen At_PP Et_PD = Binary
widen At_PP Et = Binary
widen At_PP Et_PP = PP
widen At_PP Ed_Et = Binary
widen Ad_At_PD PD = PD
widen Ad_At_PD PP = Binary
widen Ad_At_PD At_PD = At_PD
widen Ad_At_PD At = At
widen Ad_At_PD At_PP = At
widen Ad_At_PD Ad_At_PD = Ad_At_PD
widen Ad_At_PD At_Et_PD = At_PD
widen Ad_At_PD At_Et = At
widen Ad_At_PD At_Et_PP = At
widen Ad_At_PD Et_PD = PD
widen Ad_At_PD Et = Binary
widen Ad_At_PD Et_PP = Binary
widen Ad_At_PD Ed_Et = Binary
widen At_Et_PD PD = PD
widen At_Et_PD PP = Binary
widen At_Et_PD At_PD = At_PD
widen At_Et_PD At = At
widen At_Et_PD At_PP = At
widen At_Et_PD Ad_At_PD = At_PD
widen At_Et_PD At_Et_PD = At_Et_PD
widen At_Et_PD At_Et = At_Et
widen At_Et_PD At_Et_PP = At_Et
widen At_Et_PD Et_PD = Et_PD
widen At_Et_PD Et = Et
widen At_Et_PD Et_PP = Et
widen At_Et_PD Ed_Et = Et
widen At_Et PD = Binary
widen At_Et PP = Binary
widen At_Et At_PD = At
widen At_Et At = At
widen At_Et At_PP = At
widen At_Et Ad_At_PD = At
widen At_Et At_Et_PD = At_Et
widen At_Et At_Et = At_Et
widen At_Et At_Et_PP = At_Et
widen At_Et Et_PD = Et
widen At_Et Et = Et
widen At_Et Et_PP = Et
widen At_Et Ed_Et = Et
widen At_Et_PP PD = Binary
widen At_Et_PP PP = PP
widen At_Et_PP At_PD = At
widen At_Et_PP At = At
widen At_Et_PP At_PP = At_PP
widen At_Et_PP Ad_At_PD = At
widen At_Et_PP At_Et_PD = At_Et
widen At_Et_PP At_Et = At_Et
widen At_Et_PP At_Et_PP = At_Et_PP
widen At_Et_PP Et_PD = Et
widen At_Et_PP Et = Et
widen At_Et_PP Et_PP = Et_PP
widen At_Et_PP Ed_Et = Et
widen Et_PD PD = PD
widen Et_PD PP = Binary
widen Et_PD At_PD = PD
widen Et_PD At = Binary
widen Et_PD At_PP = Binary
widen Et_PD Ad_At_PD = PD
widen Et_PD At_Et_PD = Et_PD
widen Et_PD At_Et = Et
widen Et_PD At_Et_PP = Et
widen Et_PD Et_PD = Et_PD
widen Et_PD Et = Et
widen Et_PD Et_PP = Et
widen Et_PD Ed_Et = Et
widen Et PD = Binary
widen Et PP = Binary
widen Et At_PD = Binary
widen Et At = Binary
widen Et At_PP = Binary
widen Et Ad_At_PD = Binary
widen Et At_Et_PD = Et
widen Et At_Et = Et
widen Et At_Et_PP = Et
widen Et Et_PD = Et
widen Et Et = Et
widen Et Et_PP = Et
widen Et Ed_Et = Et
widen Et_PP PD = Binary
widen Et_PP PP = PP
widen Et_PP At_PD = Binary
widen Et_PP At = Binary
widen Et_PP At_PP = PP
widen Et_PP Ad_At_PD = Binary
widen Et_PP At_Et_PD = Et
widen Et_PP At_Et = Et
widen Et_PP At_Et_PP = Et_PP
widen Et_PP Et_PD = Et
widen Et_PP Et = Et
widen Et_PP Et_PP = Et_PP
widen Et_PP Ed_Et = Et
widen Ed_Et PD = Binary
widen Ed_Et PP = Binary
widen Ed_Et At_PD = Binary
widen Ed_Et At = Binary
widen Ed_Et At_PP = Binary
widen Ed_Et Ad_At_PD = Binary
widen Ed_Et At_Et_PD = Et
widen Ed_Et At_Et = Et
widen Ed_Et At_Et_PP = Et
widen Ed_Et Et_PD = Et
widen Ed_Et Et = Et
widen Ed_Et Et_PP = Et
widen Ed_Et Ed_Et = Ed_Et

data BroadCategory = ASCIIText
                   | EBCDICText
                   | ASCIINum
                   | EBCDICNum
                   | PackedDecimal
                   | BLOB
  deriving (Eq, Show)

-- heuristics
data NumTextPref = PreferNumeric | PreferTextual
data SystemPref = PreferASCII | PreferEBCDIC

broadCategory _             _            PD       = PackedDecimal
broadCategory _             _            Binary   = BLOB
broadCategory _             _            PP       = PackedDecimal
broadCategory PreferTextual _            At_PD    = ASCIIText
broadCategory PreferNumeric _            At_PD    = PackedDecimal
broadCategory _             PreferEBCDIC At       = BLOB
broadCategory _             PreferASCII  At       = ASCIIText
broadCategory PreferNumeric _            At_PP    = PackedDecimal
broadCategory PreferTextual _            At_PP    = ASCIIText
broadCategory PreferTextual _            Ad_At_PD = ASCIIText
broadCategory PreferNumeric PreferASCII  Ad_At_PD = ASCIINum
broadCategory PreferNumeric PreferEBCDIC Ad_At_PD = PackedDecimal
broadCategory PreferNumeric _            At_Et_PD = PackedDecimal
broadCategory PreferTextual PreferEBCDIC At_Et_PD = EBCDICText
broadCategory PreferTextual PreferASCII  At_Et_PD = ASCIIText
broadCategory _             PreferEBCDIC At_Et    = EBCDICText
broadCategory _             PreferASCII  At_Et    = ASCIIText
broadCategory PreferNumeric _            At_Et_PP = PackedDecimal
broadCategory PreferTextual PreferEBCDIC At_Et_PP = EBCDICText
broadCategory PreferTextual PreferASCII  At_Et_PP = ASCIIText
broadCategory PreferNumeric _            Et_PD    = PackedDecimal
broadCategory PreferTextual _            Et_PD    = EBCDICText
broadCategory _             PreferEBCDIC Et       = EBCDICText
broadCategory _             PreferASCII  Et       = BLOB
broadCategory PreferNumeric _            Et_PP    = PackedDecimal
broadCategory PreferTextual _            Et_PP    = EBCDICText
broadCategory PreferNumeric _            Ed_Et    = EBCDICNum
broadCategory PreferTextual _            Ed_Et    = EBCDICText

data BasicFoldable c e = BasicFoldOps {
         _bfLength :: c -> Int
         -- I can get away with this very specific type rather than having
         -- a `forall b . (e -> b -> b) -> b -> c -> b' but then needing
         -- to enable a language extension:
       , _bfFoldr :: (e -> [NarrowestCategory] -> [NarrowestCategory])
                  -> [NarrowestCategory]
                  -> c
                  -> [NarrowestCategory]
       }

stringOps :: BasicFoldable [Char] Char
stringOps = BasicFoldOps length foldr

byteStringOps :: BasicFoldable B.ByteString Word8
byteStringOps = BasicFoldOps B.length B.foldr

toNarrowCategoriesArray :: (CodePoint c) => BasicFoldable f c -> f -> Array Int NarrowestCategory
toNarrowCategoriesArray BasicFoldOps { _bfLength = length', _bfFoldr = foldr' } s
  = listArray (0, length' s - 1)
  $ foldr' ((:) . narrowestCategory) [] s -- `map' written as foldr

loadFile :: FilePath -> IO (Array Int NarrowestCategory)
loadFile = fmap (toNarrowCategoriesArray byteStringOps) . B.readFile
