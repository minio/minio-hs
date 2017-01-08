module Network.Minio.S3API
  ( getService
  , getLocation
  , getObject
  , putBucket
  ) where

import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Conduit as NC
import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as LBS
import Text.XML
import qualified Data.Map as M


import           Lib.Prelude

import           Network.Minio.Data
import Network.Minio.API
import Network.Minio.XmlParser

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


putBucket :: Bucket -> Location -> Minio HT.Status
putBucket bucket location = do
  resp <- executeRequest $
    requestInfo HT.methodPut (Just bucket) Nothing [] [] (PayloadSingle $ LBS.toStrict $ renderLBS def bucketConfig)
  return $ NC.responseStatus resp
    where
      root = Element (Name "CreateBucketConfiguration" (Just "http://s3.amazonaws.com/doc/2006-03-01/") Nothing) M.empty
        [ NodeElement $ Element  "LocationConstraint" M.empty
          [ NodeContent location]
        ]
      bucketConfig = Document (Prologue [] Nothing []) root []
