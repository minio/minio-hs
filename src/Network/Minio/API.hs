--
-- MinIO Haskell SDK, (C) 2017, 2018 MinIO, Inc.
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

module Network.Minio.API
  ( connect,
    S3ReqInfo (..),
    runMinio,
    executeRequest,
    buildRequest,
    mkStreamRequest,
    getLocation,
    isValidBucketName,
    checkBucketNameValidity,
    isValidObjectName,
    checkObjectNameValidity,
  )
where

import Control.Retry
  ( fullJitterBackoff,
    limitRetriesByCumulativeDelay,
    retrying,
  )
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Char as C
import qualified Data.Conduit as C
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import Lib.Prelude
import qualified Network.HTTP.Client as NClient
import Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import Network.HTTP.Types (simpleQueryToQuery)
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.Header (hHost)
import Network.Minio.APICommon
import Network.Minio.Data
import Network.Minio.Errors
import Network.Minio.Sign.V4
import Network.Minio.Utils
import Network.Minio.XmlParser

-- | Fetch bucket location (region)
getLocation :: Bucket -> Minio Region
getLocation bucket = do
  resp <-
    executeRequest $
      defaultS3ReqInfo
        { riBucket = Just bucket,
          riQueryParams = [("location", Nothing)],
          riNeedsLocation = False
        }
  parseLocation $ NC.responseBody resp

-- | Looks for region in RegionMap and updates it using getLocation if
-- absent.
discoverRegion :: S3ReqInfo -> Minio (Maybe Region)
discoverRegion ri = runMaybeT $ do
  bucket <- MaybeT $ return $ riBucket ri
  regionMay <- lift $ lookupRegionCache bucket
  maybe
    ( do
        l <- lift $ getLocation bucket
        lift $ addToRegionCache bucket l
        return l
    )
    return
    regionMay

-- | Returns the region to be used for the request.
getRegion :: S3ReqInfo -> Minio (Maybe Region)
getRegion ri = do
  ci <- asks mcConnInfo

  -- getService/makeBucket/getLocation -- don't need location
  if
      | not $ riNeedsLocation ri ->
          return $ Just $ connectRegion ci
      -- if autodiscovery of location is disabled by user
      | not $ connectAutoDiscoverRegion ci ->
          return $ Just $ connectRegion ci
      -- discover the region for the request
      | otherwise -> discoverRegion ri

getRegionHost :: Region -> Minio Text
getRegionHost r = do
  ci <- asks mcConnInfo

  if "amazonaws.com" `T.isSuffixOf` connectHost ci
    then
      maybe
        (throwIO $ MErrVRegionNotSupported r)
        return
        (H.lookup r awsRegionMap)
    else return $ connectHost ci

-- | Computes the appropriate host, path and region for the request.
--
-- For AWS, always use virtual bucket style, unless bucket has periods. For
-- MinIO and other non-AWS, default to path style.
getHostPathRegion :: S3ReqInfo -> Minio (Text, ByteString, Maybe Region)
getHostPathRegion ri = do
  ci <- asks mcConnInfo
  regionMay <- getRegion ri
  case riBucket ri of
    Nothing ->
      -- Implies a ListBuckets request.
      return (connectHost ci, "/", regionMay)
    Just bucket -> do
      regionHost <- case regionMay of
        Nothing -> return $ connectHost ci
        Just "" -> return $ connectHost ci
        Just r -> getRegionHost r
      let pathStyle =
            ( regionHost,
              getS3Path (riBucket ri) (riObject ri),
              regionMay
            )
          virtualStyle =
            ( bucket <> "." <> regionHost,
              encodeUtf8 $ "/" <> fromMaybe "" (riObject ri),
              regionMay
            )
      ( if isAWSConnectInfo ci
          then
            return $
              if bucketHasPeriods bucket
                then pathStyle
                else virtualStyle
          else return pathStyle
        )

buildRequest :: S3ReqInfo -> Minio NC.Request
buildRequest ri = do
  maybe (return ()) checkBucketNameValidity $ riBucket ri
  maybe (return ()) checkObjectNameValidity $ riObject ri

  ci <- asks mcConnInfo

  (host, path, regionMay) <- getHostPathRegion ri

  let ci' = ci {connectHost = host}
      hostHeader = (hHost, getHostAddr ci')
      ri' =
        ri
          { riHeaders = hostHeader : riHeaders ri,
            riRegion = regionMay
          }
      -- Does not contain body and auth info.
      baseRequest =
        NC.defaultRequest
          { NC.method = riMethod ri',
            NC.secure = connectIsSecure ci',
            NC.host = encodeUtf8 $ connectHost ci',
            NC.port = connectPort ci',
            NC.path = path,
            NC.requestHeaders = riHeaders ri',
            NC.queryString = HT.renderQuery False $ riQueryParams ri'
          }

  timeStamp <- liftIO Time.getCurrentTime

  let sp =
        SignParams
          (connectAccessKey ci')
          (BA.convert (encodeUtf8 $ connectSecretKey ci' :: ByteString))
          ServiceS3
          timeStamp
          (riRegion ri')
          (riPresignExpirySecs ri')
          Nothing

  -- Cases to handle:
  --
  -- 0. Handle presign URL case.
  --
  -- 1. Connection is secure: use unsigned payload
  --
  -- 2. Insecure connection, streaming signature is enabled via use of
  --    conduit payload: use streaming signature for request.
  --
  -- 3. Insecure connection, non-conduit payload: compute payload
  -- sha256hash, buffer request in memory and perform request.

  if
      | isJust (riPresignExpirySecs ri') ->
          -- case 0 from above.
          do
            let signPairs = signV4QueryParams sp baseRequest
                qpToAdd = simpleQueryToQuery signPairs
                existingQueryParams = HT.parseQuery (NC.queryString baseRequest)
                updatedQueryParams = existingQueryParams ++ qpToAdd
            return $ NClient.setQueryString updatedQueryParams baseRequest
      | isStreamingPayload (riPayload ri') && not (connectIsSecure ci') ->
          -- case 2 from above.
          do
            (pLen, pSrc) <- case riPayload ri of
              PayloadC l src -> return (l, src)
              _ -> throwIO MErrVUnexpectedPayload
            let reqFn = signV4Stream pLen sp baseRequest
            return $ reqFn pSrc
      | otherwise ->
          do
            sp' <-
              ( if connectIsSecure ci'
                  then -- case 1 described above.
                    return sp
                  else
                    ( -- case 3 described above.
                      do
                        pHash <- getPayloadSHA256Hash $ riPayload ri'
                        return $ sp {spPayloadHash = Just pHash}
                    )
                )

            let signHeaders = signV4 sp' baseRequest
            return $
              baseRequest
                { NC.requestHeaders =
                    NC.requestHeaders baseRequest ++ signHeaders,
                  NC.requestBody = getRequestBody (riPayload ri')
                }

retryAPIRequest :: Minio a -> Minio a
retryAPIRequest apiCall = do
  resE <-
    retrying retryPolicy (const shouldRetry) $
      const $
        try apiCall
  either throwIO return resE
  where
    -- Retry using the full-jitter backoff method for up to 10 mins
    -- total
    retryPolicy =
      limitRetriesByCumulativeDelay tenMins $
        fullJitterBackoff oneMilliSecond
    oneMilliSecond = 1000 -- in microseconds
    tenMins = 10 * 60 * 1000000 -- in microseconds
    -- retry on connection related failure
    shouldRetry :: Either NC.HttpException a -> Minio Bool
    shouldRetry resE =
      case resE of
        -- API request returned successfully
        Right _ -> return False
        -- API request failed with a retryable exception
        Left httpExn@(NC.HttpExceptionRequest _ exn) ->
          case (exn :: NC.HttpExceptionContent) of
            NC.ResponseTimeout -> return True
            NC.ConnectionTimeout -> return True
            NC.ConnectionFailure _ -> return True
            -- We received an unexpected exception
            _ -> throwIO httpExn
        -- We received an unexpected exception
        Left someOtherExn -> throwIO someOtherExn

executeRequest :: S3ReqInfo -> Minio (Response LByteString)
executeRequest ri = do
  req <- buildRequest ri
  mgr <- asks mcConnManager
  retryAPIRequest $ httpLbs req mgr

mkStreamRequest ::
  S3ReqInfo ->
  Minio (Response (C.ConduitM () ByteString Minio ()))
mkStreamRequest ri = do
  req <- buildRequest ri
  mgr <- asks mcConnManager
  retryAPIRequest $ http req mgr

-- Bucket name validity check according to AWS rules.
isValidBucketName :: Bucket -> Bool
isValidBucketName bucket =
  not
    ( or
        [ len < 3 || len > 63,
          any labelCheck labels,
          any labelCharsCheck labels,
          isIPCheck
        ]
    )
  where
    len = T.length bucket
    labels = T.splitOn "." bucket
    -- does label `l` fail basic checks of length and start/end?
    labelCheck l = T.length l == 0 || T.head l == '-' || T.last l == '-'
    -- does label `l` have non-allowed characters?
    labelCharsCheck l =
      isJust $
        T.find
          ( \x ->
              not
                ( C.isAsciiLower x
                    || x == '-'
                    || C.isDigit x
                )
          )
          l
    -- does label `l` have non-digit characters?
    labelNonDigits l = isJust $ T.find (not . C.isDigit) l
    labelAsNums = map (not . labelNonDigits) labels
    -- check if bucket name looks like an IP
    isIPCheck = and labelAsNums && length labelAsNums == 4

-- Throws exception iff bucket name is invalid according to AWS rules.
checkBucketNameValidity :: MonadIO m => Bucket -> m ()
checkBucketNameValidity bucket =
  unless (isValidBucketName bucket) $
    throwIO $
      MErrVInvalidBucketName bucket

isValidObjectName :: Object -> Bool
isValidObjectName object =
  T.length object > 0 && B.length (encodeUtf8 object) <= 1024

checkObjectNameValidity :: MonadIO m => Object -> m ()
checkObjectNameValidity object =
  unless (isValidObjectName object) $
    throwIO $
      MErrVInvalidObjectName object
