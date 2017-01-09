module Network.Minio.S3API
  ( getService
  , getLocation
  , getObject
  , putBucket
  ) where

import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Conduit as NC
import qualified Data.Conduit as C

import           Lib.Prelude

import qualified Data.ByteString.Lazy as LBS

import           Network.Minio.Data
import Network.Minio.API
import Network.Minio.XmlParser
import Network.Minio.XmlGenerator

getService :: Minio [BucketInfo]
getService = do
  resp <- executeRequest $
    requestInfo HT.methodGet Nothing Nothing [] [] $
    PayloadSingle ""
  parseListBuckets $ NC.responseBody resp

getLocation :: Bucket -> Minio Text
getLocation bucket = do
  resp <- executeRequest $
    requestInfo HT.methodGet (Just bucket) Nothing [("location", Nothing)] []
    (PayloadSingle "")
  parseLocation $ NC.responseBody resp

getObject :: Bucket -> Object -> HT.Query -> [HT.Header]
          -> Minio ([HT.Header], C.ResumableSource Minio ByteString)
getObject bucket object queryParams headers = do
  resp <- mkStreamRequest reqInfo
  return $ (NC.responseHeaders resp, NC.responseBody resp)
  where
    reqInfo = requestInfo HT.methodGet (Just bucket) (Just object)
              queryParams headers (PayloadSingle "")

putBucket :: Bucket -> Location -> Minio ()
putBucket bucket location = do
  resp <- executeRequest $
    requestInfo HT.methodPut (Just bucket) Nothing [] [] (PayloadSingle $ mkCreateBucketConfig bucket location)

  let httpStatus = NC.responseStatus resp
  when (httpStatus /= HT.ok200) $
    throwError $ MErrXml $ LBS.toStrict $ NC.responseBody resp
