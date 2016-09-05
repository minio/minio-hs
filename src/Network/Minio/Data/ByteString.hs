{-# LANGUAGE FlexibleInstances #-}
module Network.Minio.Data.ByteString
  (
    stripBS
  , UriEncodable(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as LB
import           Data.Char (isSpace, toUpper)
import qualified Data.Text as T
import           Numeric (showHex)

import           Lib.Prelude

stripBS :: ByteString -> ByteString
stripBS = BC8.dropWhile isSpace . fst . BC8.spanEnd isSpace

class UriEncodable s where
  uriEncode :: Bool -> s -> ByteString

instance UriEncodable [Char] where
  uriEncode encodeSlash payload =
    LB.toStrict $ BB.toLazyByteString $ mconcat $
    map (flip uriEncodeChar encodeSlash) payload

instance UriEncodable ByteString where
  -- assumes that uriEncode is passed ASCII encoded strings.
  uriEncode encodeSlash bs =
    uriEncode encodeSlash $ BC8.unpack bs

instance UriEncodable Text where
  uriEncode encodeSlash txt =
    uriEncode encodeSlash $ T.unpack txt

-- | URI encode a char according to AWS S3 signing rules - see
-- UriEncode() at
-- https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
uriEncodeChar :: Char -> Bool -> BB.Builder
uriEncodeChar '/' True = BB.byteString "%2F"
uriEncodeChar '/' False = BB.char7 '/'
uriEncodeChar ch _
  | (ch >= 'A' && ch <= 'Z')
    || (ch >= 'a' && ch <= 'z')
    || (ch >= '0' && ch <= '9')
    || (ch == '_')
    || (ch == '-')
    || (ch == '.')
    || (ch == '~') = BB.char7 ch
  | otherwise = mconcat $ map f $ B.unpack $ encodeUtf8 $ T.singleton ch
  where
    f :: Word8 -> BB.Builder
    f n = BB.char7 '%' <> BB.string7 hexStr
      where
        hexStr = map toUpper $ showHex q $ showHex r ""
        (q, r) = divMod (fromIntegral n) (16::Word8)
