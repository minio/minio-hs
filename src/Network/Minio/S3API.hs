module Network.Minio.S3API
  ( getService
  , getLocation
  , getObject
  , putBucket
  , deleteBucket
  , deleteObject
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

status204 :: HT.Status
status204 = HT.Status{ HT.statusCode = 204, HT.statusMessage = "No Content" }

getService :: Minio [BucketInfo]
getService = do
  resp <- executeRequest $
    requestInfo HT.methodGet Nothing Nothing [] [] Nothing
  parseListBuckets $ NC.responseBody resp

getLocation :: Bucket -> Minio Text
getLocation bucket = do
  resp <- executeRequest $
    requestInfo HT.methodGet (Just bucket) Nothing [("location", Nothing)] []
    Nothing
  parseLocation $ NC.responseBody resp

getObject :: Bucket -> Object -> HT.Query -> [HT.Header]
          -> Minio ([HT.Header], C.ResumableSource Minio ByteString)
getObject bucket object queryParams headers = do
  resp <- mkStreamRequest reqInfo
  let httpStatusCode = HT.statusCode $ NC.responseStatus resp
  if httpStatusCode >= 200 && httpStatusCode < 300
    then return $ (NC.responseHeaders resp, NC.responseBody resp)
    else do errMsg <- NC.lbsResponse resp
            throwError $ MErrXml $ LBS.toStrict $ NC.responseBody errMsg
  where
    reqInfo = requestInfo HT.methodGet (Just bucket) (Just object)
              queryParams headers Nothing

putBucket :: Bucket -> Location -> Minio ()
putBucket bucket location = do
  resp <- executeRequest $
    requestInfo HT.methodPut (Just bucket) Nothing [] [] $
    Just $ mkCreateBucketConfig bucket location

  let httpStatus = NC.responseStatus resp
  when (httpStatus /= HT.ok200) $
    throwError $ MErrXml $ LBS.toStrict $ NC.responseBody resp

deleteBucket :: Bucket -> Minio ()
deleteBucket bucket = do
  resp <- executeRequest $
    requestInfo HT.methodDelete (Just bucket) Nothing [] [] Nothing
  let httpStatus = NC.responseStatus resp
  when (httpStatus /= status204) $
    throwError $ MErrXml $ LBS.toStrict $ NC.responseBody resp

deleteObject :: Bucket -> Object -> Minio ()
deleteObject bucket object = do
  resp <- executeRequest $
    requestInfo HT.methodDelete (Just bucket) (Just object) [] [] Nothing
  let httpStatus = NC.responseStatus resp
  when (httpStatus /= status204) $
    throwError $ MErrXml $ LBS.toStrict $ NC.responseBody resp
