--
-- Minio Haskell SDK, (C) 2017 Minio, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Network.Minio.Data.Crypto
  (
    hashSHA256
  , hashSHA256FromSource

  , hashMD5
  , hashMD5FromSource

  , hmacSHA256
  , hmacSHA256RawBS
  , digestToBS
  , digestToBase16
  ) where

import           Crypto.Hash (SHA256(..), MD5(..), hashWith, Digest)
import           Crypto.Hash.Conduit (sinkHash)
import           Crypto.MAC.HMAC (hmac, HMAC)
import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.Conduit as C

import           Lib.Prelude

hashSHA256 :: ByteString -> ByteString
hashSHA256 = digestToBase16 . hashWith SHA256

hashSHA256FromSource :: Monad m => C.Producer m ByteString -> m ByteString
hashSHA256FromSource src = do
  digest <- src C.$$ sinkSHA256Hash
  return $ digestToBase16 digest
  where
    -- To help with type inference
    sinkSHA256Hash :: Monad m => C.Consumer ByteString m (Digest SHA256)
    sinkSHA256Hash = sinkHash

hashMD5 :: ByteString -> ByteString
hashMD5 = digestToBase16 . hashWith MD5

hashMD5FromSource :: Monad m => C.Producer m ByteString -> m ByteString
hashMD5FromSource src = do
  digest <- src C.$$ sinkMD5Hash
  return $ digestToBase16 digest
  where
    -- To help with type inference
    sinkMD5Hash :: Monad m => C.Consumer ByteString m (Digest MD5)
    sinkMD5Hash = sinkHash

hmacSHA256 :: ByteString -> ByteString -> HMAC SHA256
hmacSHA256 message key = hmac key message

hmacSHA256RawBS :: ByteString -> ByteString -> ByteString
hmacSHA256RawBS message key = convert $ hmacSHA256 message key

digestToBS :: ByteArrayAccess a => a -> ByteString
digestToBS = convert

digestToBase16 :: ByteArrayAccess a => a -> ByteString
digestToBase16 = convertToBase Base16
