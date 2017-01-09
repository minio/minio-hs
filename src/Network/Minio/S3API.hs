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


import           Network.Minio.Data
import Network.Minio.API
import Network.Minio.XmlParser
import Network.Minio.XmlGenerator

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
  return $ (NC.responseHeaders resp, NC.responseBody resp)
  where
    reqInfo = requestInfo HT.methodGet (Just bucket) (Just object)
              queryParams headers Nothing

putBucket :: Bucket -> Location -> Minio ()
putBucket bucket location = do
  void $ executeRequest $
    requestInfo HT.methodPut (Just bucket) Nothing [] [] $
    Just $ mkCreateBucketConfig bucket location

deleteBucket :: Bucket -> Minio ()
deleteBucket bucket = do
  void $ executeRequest $
    requestInfo HT.methodDelete (Just bucket) Nothing [] [] Nothing

deleteObject :: Bucket -> Object -> Minio ()
deleteObject bucket object = do
  void $ executeRequest $
    requestInfo HT.methodDelete (Just bucket) (Just object) [] [] Nothing
