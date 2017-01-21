{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Network.Minio.Data where

import qualified Data.ByteString as B
import           Network.HTTP.Client (defaultManagerSettings, HttpException)
import           Network.HTTP.Types (Method, Header, Query)
import qualified Network.HTTP.Conduit as NC
import Data.Default (Default(..))
import qualified Network.HTTP.Types as HT

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Base

import Text.XML

import           Lib.Prelude

-- | Connection Info data type. Use the Default instance to create
-- connection info for your service.
data ConnectInfo = ConnectInfo {
    connectHost :: Text
  , connectPort :: Int
  , connectAccessKey :: Text
  , connectSecretKey :: Text
  , connectIsSecure :: Bool
  } deriving (Eq, Show)

instance Default ConnectInfo where
  def = ConnectInfo "localhost" 9000 "minio" "minio123" False

-- |
-- Represents a bucket in the object store
type Bucket = Text

-- |
-- Represents an object name
type Object = Text

-- |
-- Represents a region
-- TODO: This could be a Sum Type with all defined regions for AWS.
type Region = Text

-- | A type alias to represent an Entity-Tag returned by S3-compatible
-- APIs.
type ETag = Text

-- |
-- BucketInfo returned for list buckets call
data BucketInfo = BucketInfo {
    biName :: Bucket
  , biCreationDate :: UTCTime
  } deriving (Show, Eq)

-- | A type alias to represent a part-number for multipart upload
type PartNumber = Int16

-- | A type alias to represent an upload-id for multipart upload
type UploadId = Text

-- | A data-type to represent info about a part
data PartInfo = PartInfo PartNumber ETag
  deriving (Show, Eq)

instance Ord PartInfo where
  (PartInfo a _) `compare` (PartInfo b _) = a `compare` b


data ListObjectsResult = ListObjectsResult {
    lorHasMore :: Bool
  , lorNextToken :: Maybe Text
  , lorObjects :: [ObjectInfo]
  , lorCPrefixes :: [Text]
  } deriving (Show, Eq)

data ObjectInfo = ObjectInfo {
    oiObject :: Object
  , oiModTime :: UTCTime
  , oiETag :: ETag
  , oiSize :: Int64
  } deriving (Show, Eq)

data Payload = PayloadBS ByteString
             | PayloadH Handle
                        Int64 -- offset
                        Int64 -- size

instance Default Payload where
  def = PayloadBS ""

data RequestInfo = RequestInfo {
    riMethod :: Method
  , riBucket :: Maybe Bucket
  , riObject :: Maybe Object
  , riQueryParams :: Query
  , riHeaders :: [Header]
  , riPayload :: Payload
  , riPayloadHash :: ByteString
  , riRegion :: Maybe Region
  }

instance Default RequestInfo where
  def = RequestInfo HT.methodGet def def def def def "" def

getPathFromRI :: RequestInfo -> ByteString
getPathFromRI ri = B.concat $ parts
  where
    objPart = maybe [] (\o -> ["/", encodeUtf8 o]) $ riObject ri
    parts = maybe ["/"] (\b -> "/" : encodeUtf8 b : objPart) $ riBucket ri

getRegionFromRI :: RequestInfo -> Text
getRegionFromRI ri = maybe "us-east-1" identity (riRegion ri)

-- | Various validation errors
data MErrV = MErrVSinglePUTSizeExceeded Int64
           | MErrVETagHeaderNotFound
  deriving (Show)

-- |
-- Minio Error data type for various errors/exceptions caught and
-- returned.
data MinioErr = MErrMsg ByteString -- generic
              | MErrHttp HttpException -- http exceptions
              | MErrXml ByteString -- XML parsing/generation errors
              | MErrService ByteString -- error response from service
              | MErrValidation MErrV -- client-side validation errors
              | MErrIO IOException -- exceptions while working with files
  deriving (Show)

newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (ExceptT MinioErr (ResourceT IO)) a
  }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
    , MonadError MinioErr
    , MonadThrow
    , MonadBase IO
    , MonadResource
    )

instance MonadBaseControl IO Minio where
  type StM Minio a = Either MinioErr a
  liftBaseWith f = Minio $ liftBaseWith $ \q -> f (q . unMinio)
  restoreM = Minio . restoreM

-- | MinioConn holds connection info and a connection pool
data MinioConn = MinioConn {
    mcConnInfo :: ConnectInfo
  , mcConnManager :: NC.Manager
  }

-- | Takes connection information and returns a connection object to
-- be passed to @runMinio
connect :: ConnectInfo -> IO MinioConn
connect ci = do
  mgr <- NC.newManager defaultManagerSettings
  return $ MinioConn ci mgr

-- | Run the Minio action and return the result or error.
runMinio :: ConnectInfo -> Minio a -> ResourceT IO (Either MinioErr a)
runMinio ci m = do
  conn <- liftIO $ connect ci
  runExceptT . flip runReaderT conn . unMinio $ m

s3Name :: Text -> Name
s3Name s = Name s (Just "http://s3.amazonaws.com/doc/2006-03-01/") Nothing
