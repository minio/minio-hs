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

module Network.Minio.Errors where

import           Control.Exception
import qualified Network.HTTP.Conduit as NC

import           Lib.Prelude


---------------------------------
-- Errors
---------------------------------
-- | Various validation errors
data MErrV = MErrVSinglePUTSizeExceeded Int64
           | MErrVPutSizeExceeded Int64
           | MErrVETagHeaderNotFound
           | MErrVInvalidObjectInfoResponse
           | MErrVInvalidSrcObjSpec Text
           | MErrVInvalidSrcObjByteRange (Int64, Int64)
           | MErrVCopyObjSingleNoRangeAccepted
  deriving (Show, Eq)

-- | Errors thrown by the library
data MinioErr = ME MError
              | MEHttp NC.HttpException
              | MEFile IOException
  deriving (Show)

instance Exception MinioErr

-- | Library internal errors
data MError = XMLParseError Text
            | ResponseError (NC.Response LByteString)
            | ValidationError MErrV
  deriving (Show, Eq)

instance Exception MError
