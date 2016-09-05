module Network.Minio.Data.Crypto
  (
    hashSHA256
  , hmacSHA256
  ) where

import Crypto.Hash (SHA256(..), hashWith, Digest)
import Crypto.MAC.HMAC (hmac, HMAC(hmacGetDigest))

import Lib.Prelude

hashSHA256 :: ByteString -> ByteString
hashSHA256 = show . hashWith SHA256

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 message key =
  show (hmacGetDigest (hmac key message) :: Digest SHA256)
