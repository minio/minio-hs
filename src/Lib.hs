{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
    ( someFunc
    , hmacSha256
    , sha256Hash
    ) where

import Lib.Prelude

import Crypto.Hash (SHA256, Digest, hash)
import Crypto.MAC.HMAC (hmac, HMAC(hmacGetDigest))

-- | Prints someFunc
--
-- >>> someFunc 10
-- someFunc
someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: Text)

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key message =
  show (hmacGetDigest (hmac key message) :: Digest SHA256)

sha256Hash :: ByteString -> ByteString
sha256Hash payload = show (hash payload :: Digest SHA256)

{-


public static String UriEncode(CharSequence input, boolean encodeSlash) {
          StringBuilder result = new StringBuilder();
          for (int i = 0; i < input.length(); i++) {
              char ch = input.charAt(i);
              if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
                  (ch >= '0' && ch <= '9') || ch == '_' || ch == '-' || ch == '~' || ch == '.') {
                  result.append(ch);
              } else if (ch == '/') {
                  result.append(encodeSlash ? "%2F" : ch);
              } else {
                  result.append(toHexUTF8(ch));
              }
          }
          return result.toString();
      }
ch 65536 -> %10000
-}
uriEncode :: Text -> Bool -> ByteString
uriEncode payload encodeSlash = 