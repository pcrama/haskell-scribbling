module Password
( Password -- Type only, to avoid getting at the full SHA1
, SHA1Prefix(..)
, compareWithSha1
, isPasswordChar
, mkPassword
, sha1Prefix
)
where

import Control.Monad (guard)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Char as C
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T
import Data.Word (Word8)


data Password = Password L.ByteString SHA1Prefix Int


instance Show Password where
  show (Password _ _ l) = Prelude.replicate l '*'


compareWithSha1 :: Password -> L.ByteString -> Bool
-- Password p _ _ -> precomputed SHA1 is stored lower case
compareWithSha1 (Password p _ _) s = p == L.map C.toLower s


data SHA1Prefix = SHA1Prefix Word8 Word8 Word8
  deriving (Show, Eq)


sha1Prefix :: Password -> SHA1Prefix
sha1Prefix (Password _ p _) = p


mkPassword :: T.Text -> Maybe Password
mkPassword t = do
  guard $ T.all isPasswordChar t
  let lb = L.pack $ T.unpack t
  let s = map C.toLower . SHA.showDigest . SHA.sha1 $ lb
  guard $ all C.isHexDigit s
  case s of
    (a:b:c:d:e:_) ->
      Just $ Password (L.pack s)
                      (SHA1Prefix (toWord8 a b)
                                  (toWord8 c d)
                                  (toWord8 e '0'))
                    $ T.length t
    _ -> Nothing
  where toWord8 x y = fromIntegral $ 16 * (C.digitToInt x)
                                   + C.digitToInt y


isPasswordChar :: Char -> Bool
isPasswordChar c = ' ' < c && c < '\x80'
