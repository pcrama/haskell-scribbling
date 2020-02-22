module Password
( Password -- Type only, to avoid getting at the cleartext
, SHA1Prefix(..)
, sha1Prefix
, compareWithSha1
, mkPassword
)
where

import qualified Data.ByteString.Lazy as L
import qualified Data.Char as C
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T
import Data.Word (Word8)

newtype Password = Password T.Text

instance Show Password where
  show (Password p) = Prelude.replicate (T.length p) '*'

-- Data.Text.unpack :: Text -> String
--  ... String -> [GHC.Word.Word8]
-- Data.ByteString.Lazy.pack :: [GHC.Word.Word8] -> ByteString
_toLazyByteString :: Password -> L.ByteString
_toLazyByteString (Password p) =
  L.pack . map ordWord8 . T.unpack $ p
  where ordWord8 = fromIntegral . C.ord

_sha1String :: Password -> String
_sha1String = SHA.showDigest . SHA.sha1 . _toLazyByteString

compareWithSha1 :: Password -> String -> Bool
compareWithSha1 p s = map C.toLower s == map C.toLower (_sha1String p)

data SHA1Prefix = SHA1Prefix Word8 Word8 Word8
  deriving (Show, Eq)

sha1Prefix :: Password -> Maybe SHA1Prefix
sha1Prefix p
  | all C.isHexDigit s = Just $ SHA1Prefix (toWord8 a b)
                                           (toWord8 c d)
                                           (toWord8 e f)
  | otherwise = Nothing
  where s = _sha1String p
        (a:b:c:d:e:f:_) = s
        toWord8 x y = fromIntegral $ 16 * (C.digitToInt x)
                                   + C.digitToInt y

mkPassword :: T.Text -> Maybe Password
mkPassword t
  | T.all C.isLatin1 t = Just $ Password t
  | otherwise = Nothing
