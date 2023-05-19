--
-- MinIO Haskell SDK, (C) 2017-2023 MinIO, Inc.
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

module Network.Minio.Utils where

import qualified Conduit as C
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive (mk, original)
import qualified Data.Conduit.Binary as CB
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Time
  ( defaultTimeLocale,
    parseTimeM,
    rfc822DateFormat,
  )
import Lib.Prelude
import Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as Hdr
import Network.Minio.Data.ByteString
import Network.Minio.JsonParser (parseErrResponseJSON)
import Network.Minio.XmlCommon (parseErrResponse)
import qualified System.IO as IO
import qualified UnliftIO as U
import qualified UnliftIO.Async as A

allocateReadFile ::
  (MonadUnliftIO m, R.MonadResource m) =>
  FilePath ->
  m (R.ReleaseKey, Handle)
allocateReadFile fp = do
  (rk, hdlE) <- R.allocate (openReadFile fp) cleanup
  either (\(e :: U.IOException) -> throwIO e) (return . (rk,)) hdlE
  where
    openReadFile f = U.try $ IO.openBinaryFile f IO.ReadMode
    cleanup = either (const $ return ()) IO.hClose

-- | Queries the file size from the handle. Catches any file operation
-- exceptions and returns Nothing instead.
getFileSize ::
  (MonadUnliftIO m) =>
  Handle ->
  m (Maybe Int64)
getFileSize h = do
  resE <- liftIO $ try $ fromIntegral <$> IO.hFileSize h
  case resE of
    Left (_ :: U.IOException) -> return Nothing
    Right s -> return $ Just s

-- | Queries if handle is seekable. Catches any file operation
-- exceptions and return False instead.
isHandleSeekable ::
  (R.MonadResource m) =>
  Handle ->
  m Bool
isHandleSeekable h = do
  resE <- liftIO $ try $ IO.hIsSeekable h
  case resE of
    Left (_ :: U.IOException) -> return False
    Right v -> return v

-- | Helper function that opens a handle to the filepath and performs
-- the given action on it. Exceptions of type MError are caught and
-- returned - both during file handle allocation and when the action
-- is run.
withNewHandle ::
  (MonadUnliftIO m, R.MonadResource m) =>
  FilePath ->
  (Handle -> m a) ->
  m (Either U.IOException a)
withNewHandle fp fileAction = do
  -- opening a handle can throw MError exception.
  handleE <- try $ allocateReadFile fp
  either (return . Left) doAction handleE
  where
    doAction (rkey, h) = do
      -- fileAction may also throw MError exception, so we catch and
      -- return it.
      resE <- try $ fileAction h
      R.release rkey
      return resE

mkHeaderFromPairs :: [(ByteString, ByteString)] -> [HT.Header]
mkHeaderFromPairs = map (first mk)

lookupHeader :: HT.HeaderName -> [HT.Header] -> Maybe ByteString
lookupHeader hdr = listToMaybe . map snd . filter (\(h, _) -> h == hdr)

getETagHeader :: [HT.Header] -> Maybe Text
getETagHeader hs = decodeUtf8Lenient <$> lookupHeader Hdr.hETag hs

getMetadata :: [HT.Header] -> [(Text, Text)]
getMetadata =
  map (\(x, y) -> (decodeUtf8Lenient $ original x, decodeUtf8Lenient $ stripBS y))

-- | If the given header name has the @X-Amz-Meta-@ prefix, it is
-- stripped and a Just is returned.
userMetadataHeaderNameMaybe :: Text -> Maybe Text
userMetadataHeaderNameMaybe k =
  let prefix = T.toCaseFold "X-Amz-Meta-"
      n = T.length prefix
   in if T.toCaseFold (T.take n k) == prefix
        then Just (T.drop n k)
        else Nothing

toMaybeMetadataHeader :: (Text, Text) -> Maybe (Text, Text)
toMaybeMetadataHeader (k, v) =
  (,v) <$> userMetadataHeaderNameMaybe k

getNonUserMetadataMap :: [(Text, Text)] -> H.HashMap Text Text
getNonUserMetadataMap =
  H.fromList
    . filter
      ( isNothing
          . userMetadataHeaderNameMaybe
          . fst
      )

addXAmzMetaPrefix :: Text -> Text
addXAmzMetaPrefix s
  | isJust (userMetadataHeaderNameMaybe s) = s
  | otherwise = "X-Amz-Meta-" <> s

mkHeaderFromMetadata :: [(Text, Text)] -> [HT.Header]
mkHeaderFromMetadata = map (\(x, y) -> (mk $ encodeUtf8 $ addXAmzMetaPrefix x, encodeUtf8 y))

-- | This function collects all headers starting with `x-amz-meta-`
-- and strips off this prefix, and returns a map.
getUserMetadataMap :: [(Text, Text)] -> H.HashMap Text Text
getUserMetadataMap =
  H.fromList
    . mapMaybe toMaybeMetadataHeader

getHostHeader :: (ByteString, Int) -> ByteString
getHostHeader (host_, port_) =
  if port_ == 80 || port_ == 443
    then host_
    else host_ <> ":" <> show port_

getLastModifiedHeader :: [HT.Header] -> Maybe UTCTime
getLastModifiedHeader hs = do
  modTimebs <- decodeUtf8Lenient <$> lookupHeader Hdr.hLastModified hs
  parseTimeM True defaultTimeLocale rfc822DateFormat (T.unpack modTimebs)

getContentLength :: [HT.Header] -> Maybe Int64
getContentLength hs = do
  nbs <- decodeUtf8Lenient <$> lookupHeader Hdr.hContentLength hs
  fst <$> either (const Nothing) Just (decimal nbs)

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

isSuccessStatus :: HT.Status -> Bool
isSuccessStatus sts =
  let s = HT.statusCode sts
   in (s >= 200 && s < 300)

httpLbs ::
  (MonadIO m) =>
  NC.Request ->
  NC.Manager ->
  m (NC.Response LByteString)
httpLbs req mgr = do
  respE <- liftIO $ tryHttpEx $ NC.httpLbs req mgr
  resp <- either throwIO return respE
  unless (isSuccessStatus $ NC.responseStatus resp) $
    case contentTypeMay resp of
      Just "application/xml" -> do
        sErr <- parseErrResponse $ NC.responseBody resp
        throwIO sErr
      Just "application/json" -> do
        sErr <- parseErrResponseJSON $ NC.responseBody resp
        throwIO sErr
      _ ->
        throwIO $
          NC.HttpExceptionRequest req $
            NC.StatusCodeException (void resp) (showBS resp)

  return resp
  where
    tryHttpEx ::
      IO (NC.Response LByteString) ->
      IO (Either NC.HttpException (NC.Response LByteString))
    tryHttpEx = try
    contentTypeMay resp =
      lookupHeader Hdr.hContentType $
        NC.responseHeaders resp

http ::
  (MonadUnliftIO m, R.MonadResource m) =>
  NC.Request ->
  NC.Manager ->
  m (Response (C.ConduitT () ByteString m ()))
http req mgr = do
  respE <- tryHttpEx $ NC.http req mgr
  resp <- either throwIO return respE
  unless (isSuccessStatus $ NC.responseStatus resp) $
    case contentTypeMay resp of
      Just "application/xml" -> do
        respBody <- C.connect (NC.responseBody resp) CB.sinkLbs
        sErr <- parseErrResponse respBody
        throwIO sErr
      _ -> do
        content <- LB.toStrict . NC.responseBody <$> NC.lbsResponse resp
        throwIO $
          NC.HttpExceptionRequest req $
            NC.StatusCodeException (void resp) content

  return resp
  where
    tryHttpEx ::
      (MonadUnliftIO m) =>
      m a ->
      m (Either NC.HttpException a)
    tryHttpEx = try
    contentTypeMay resp =
      lookupHeader Hdr.hContentType $
        NC.responseHeaders resp

-- Similar to mapConcurrently but limits the number of threads that
-- can run using a quantity semaphore.
limitedMapConcurrently ::
  (MonadUnliftIO m) =>
  Int ->
  (t -> m a) ->
  [t] ->
  m [a]
limitedMapConcurrently 0 _ _ = return []
limitedMapConcurrently count act args = do
  t' <- U.newTVarIO count
  threads <- mapM (A.async . wThread t') args
  mapM A.wait threads
  where
    wThread t arg =
      U.bracket_ (waitSem t) (signalSem t) $ act arg
    -- quantity semaphore implementation using TVar
    waitSem t = U.atomically $ do
      v <- U.readTVar t
      if v > 0
        then U.writeTVar t (v - 1)
        else U.retrySTM
    signalSem t = U.atomically $ do
      v <- U.readTVar t
      U.writeTVar t (v + 1)

-- helper function to 'drop' empty optional parameter.
mkQuery :: Text -> Maybe Text -> Maybe (Text, Text)
mkQuery k mv = (k,) <$> mv

-- helper function to build query parameters that are optional.
-- don't use it with mandatory query params with empty value.
mkOptionalParams :: [(Text, Maybe Text)] -> HT.Query
mkOptionalParams params = HT.toQuery $ uncurry mkQuery <$> params

-- | Conduit that rechunks bytestrings into the given chunk
-- lengths. Stops after given chunk lengths are yielded. Stops if
-- there are no more chunks to yield or if a shorter chunk is
-- received. Does not throw any errors.
chunkBSConduit :: (Monad m) => [Int] -> C.ConduitM ByteString ByteString m ()
chunkBSConduit [] = return ()
chunkBSConduit (s : ss) = do
  bs <- fmap LB.toStrict $ C.takeCE s C..| C.sinkLazy
  if
      | B.length bs == s -> C.yield bs >> chunkBSConduit ss
      | B.length bs > 0 -> C.yield bs
      | otherwise -> return ()
