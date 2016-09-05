module Network.Minio.Data
  (
    MinioClient(..)
  , RequestInfo(..)
  , Bucket
  , Object
  , getPathFromRI
  ) where

import qualified Data.ByteString as B
import           Network.HTTP.Types (Method, Header, Query)

import           Lib.Prelude

data MinioClient = MinioClient {
    mcEndPointHost :: Text
  , mcEndPointPort :: Int
  , mcAccessKey :: Text
  , mcSecretKey :: Text
  , mcIsSecure :: Bool
  , mcRegion :: Text
  } deriving (Eq, Show)

type Bucket = ByteString
type Object = Text

data RequestInfo = RequestInfo {
    method :: Method
  , bucket :: Maybe Bucket
  , object :: Maybe Object
  , queryParams :: Query
  , headers :: [Header]
  , payload :: ByteString
  , payloadHash :: ByteString
  }

getPathFromRI :: RequestInfo -> ByteString
getPathFromRI ri = B.concat $ parts
  where
    objPart = maybe [] (\o -> ["/", encodeUtf8 o]) $ object ri
    parts = maybe ["/"] (\b -> "/" : b : objPart) $ bucket ri
