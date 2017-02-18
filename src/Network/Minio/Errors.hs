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
