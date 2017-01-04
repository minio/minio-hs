module Network.Minio.S3API
  ( getService
  , getLocation
  ) where

import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Conduit as NC


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

-- getObject ::
