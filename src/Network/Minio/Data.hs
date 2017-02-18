{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Network.Minio.Data where

import           Control.Monad.Base
import qualified Control.Monad.Catch as MC
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import           Data.Default (Default(..))
import qualified Data.Text as T
import           Network.HTTP.Client (defaultManagerSettings)
import qualified Network.HTTP.Conduit as NC
import           Network.HTTP.Types (Method, Header, Query)
import qualified Network.HTTP.Types as HT
import           Text.XML

import           Lib.Prelude

import           Network.Minio.Errors
import           Network.Minio.Utils

-- | Connection Info data type. Use the Default instance to create
-- connection info for your service.
data ConnectInfo = ConnectInfo {
    connectHost :: Text
  , connectPort :: Int
  , connectAccessKey :: Text
  , connectSecretKey :: Text
  , connectIsSecure :: Bool
  , connectRegion :: Region
  } deriving (Eq, Show)

instance Default ConnectInfo where
  def = ConnectInfo "localhost" 9000 "minio" "minio123" False "us-east-1"

-- |
-- Default aws ConnectInfo. Credentials should be supplied before use.
awsCI :: ConnectInfo
awsCI = def {
    connectHost = "s3.amazonaws.com"
  , connectPort = 443
  , connectAccessKey = ""
  , connectSecretKey = ""
  , connectIsSecure = True
  }

-- |
-- Default minio play server ConnectInfo. Credentials are already filled.
minioPlayCI :: ConnectInfo
minioPlayCI = def {
    connectHost = "play.minio.io"
  , connectPort = 9000
  , connectAccessKey = "Q3AM3UQ867SPQQA43P2F"
  , connectSecretKey = "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG"
  , connectIsSecure = True
  }

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


-- | Represents result from a listing of object parts of an ongoing
-- multipart upload.
data ListPartsResult = ListPartsResult {
    lprHasMore :: Bool
  , lprNextPart :: Maybe Int
  , lprParts :: [ListPartInfo]
 } deriving (Show, Eq)


-- | Represents information about an object part in an ongoing
-- multipart upload.
data ListPartInfo = ListPartInfo {
    piNumber :: PartNumber
  , piETag :: ETag
  , piSize :: Int64
  , piModTime :: UTCTime
  } deriving (Show, Eq)

-- | Represents result from a listing of incomplete uploads to a
-- bucket.
data ListUploadsResult = ListUploadsResult {
    lurHasMore :: Bool
  , lurNextKey :: Maybe Text
  , lurNextUpload :: Maybe Text
  , lurUploads :: [UploadInfo]
  , lurCPrefixes :: [Text]
  } deriving (Show, Eq)

-- | Represents information about a multipart upload.
data UploadInfo = UploadInfo {
    uiKey :: Object
  , uiUploadId :: UploadId
  , uiInitTime :: UTCTime
  } deriving (Show, Eq)

-- | Represents result from a listing of objects in a bucket.
data ListObjectsResult = ListObjectsResult {
    lorHasMore :: Bool
  , lorNextToken :: Maybe Text
  , lorObjects :: [ObjectInfo]
  , lorCPrefixes :: [Text]
  } deriving (Show, Eq)

-- | Represents information about an object.
data ObjectInfo = ObjectInfo {
    oiObject :: Object
  , oiModTime :: UTCTime
  , oiETag :: ETag
  , oiSize :: Int64
  } deriving (Show, Eq)

data CopyPartSource = CopyPartSource {
    cpSource :: Text -- | formatted like "/sourceBucket/sourceObject"
  , cpSourceRange :: Maybe (Int64, Int64) -- | (0, 9) means first ten
                                          -- bytes of the source
                                          -- object
  , cpSourceIfMatch :: Maybe Text
  , cpSourceIfNoneMatch :: Maybe Text
  , cpSourceIfUnmodifiedSince :: Maybe UTCTime
  , cpSourceIfModifiedSince :: Maybe UTCTime
  } deriving (Show, Eq)

instance Default CopyPartSource where
  def = CopyPartSource "" def def def def def

cpsToHeaders :: CopyPartSource -> [HT.Header]
cpsToHeaders cps = ("x-amz-copy-source", encodeUtf8 $ cpSource cps) :
                   (rangeHdr ++ (zip names values))
  where
    names = ["x-amz-copy-source-if-match", "x-amz-copy-source-if-none-match",
             "x-amz-copy-source-if-unmodified-since",
             "x-amz-copy-source-if-modified-since"]
    values = concatMap (maybeToList . fmap encodeUtf8 . (cps &))
             [cpSourceIfMatch, cpSourceIfNoneMatch,
              fmap formatRFC1123 . cpSourceIfUnmodifiedSince,
              fmap formatRFC1123 . cpSourceIfModifiedSince]
    rangeHdr = ("x-amz-copy-source-range",)
             . HT.renderByteRanges
             . (:[])
             . uncurry HT.ByteRangeFromTo
           <$> (map (both fromIntegral) $
                maybeToList $ cpSourceRange cps)

-- | Extract the source bucket and source object name. TODO: validate
-- the bucket and object name extracted.
cpsToObject :: CopyPartSource -> Maybe (Bucket, Object)
cpsToObject cps = do
  [_, bucket, object] <- Just splits
  return (bucket, object)
  where
    splits = T.splitOn "/" $ cpSource cps

-- | Represents different kinds of payload that are used with S3 API
-- requests.
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

newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (ResourceT IO) a
  }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
    , MonadThrow
    , MonadCatch
    , MonadBase IO
    , MonadResource
    )

instance MonadBaseControl IO Minio where
  type StM Minio a = a
  liftBaseWith f = Minio $ liftBaseWith $ \q -> f (q . unMinio)
  restoreM = Minio . restoreM

-- | MinioConn holds connection info and a connection pool
data MinioConn = MinioConn {
    mcConnInfo :: ConnectInfo
  , mcConnManager :: NC.Manager
  }

-- | Takes connection information and returns a connection object to
-- be passed to 'runMinio'
connect :: ConnectInfo -> IO MinioConn
connect ci = do
  let settings = bool defaultManagerSettings NC.tlsManagerSettings $
        connectIsSecure ci
  mgr <- NC.newManager settings
  return $ MinioConn ci mgr

-- | Run the Minio action and return the result or an error.
runMinio :: ConnectInfo -> Minio a -> ResourceT IO (Either MinioErr a)
runMinio ci m = do
  conn <- liftIO $ connect ci
  flip runReaderT conn . unMinio $
    (m >>= (return . Right)) `MC.catches`
    [MC.Handler handlerME, MC.Handler handlerHE, MC.Handler handlerFE]
  where
    handlerME = return . Left . ME
    handlerHE = return . Left . MEHttp
    handlerFE = return . Left . MEFile

s3Name :: Text -> Name
s3Name s = Name s (Just "http://s3.amazonaws.com/doc/2006-03-01/") Nothing
