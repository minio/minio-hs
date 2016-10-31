module Network.Minio.Data.Crypto
  (
    hashSHA256
  , hmacSHA256
  , hmacSHA256RawBS
  , digestToBS
  , digestToBase16
  ) where

import Crypto.Hash (SHA256(..), hashWith)
import Crypto.MAC.HMAC (hmac, HMAC)
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))

import Lib.Prelude

hashSHA256 :: ByteString -> ByteString
hashSHA256 = convertToBase Base16 . hashWith SHA256

hmacSHA256 :: ByteString -> ByteString -> HMAC SHA256
hmacSHA256 message key = hmac key message

hmacSHA256RawBS :: ByteString -> ByteString -> ByteString
hmacSHA256RawBS message key = convert $ hmacSHA256 message key

digestToBS :: ByteArrayAccess a => a -> ByteString
digestToBS = convert

digestToBase16 :: ByteArrayAccess a => a -> ByteString
digestToBase16 = convertToBase Base16
