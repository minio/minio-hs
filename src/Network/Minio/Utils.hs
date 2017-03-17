--
-- Minio Haskell SDK, (C) 2017 Minio, Inc.
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

import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.QSem as Q
import qualified Control.Exception.Lifted as ExL
import qualified Control.Monad.Catch as MC
import           Control.Monad.Trans.Control (liftBaseOp_, StM)
import qualified Control.Monad.Trans.Resource as R

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Text.Read (decimal)
import           Data.Time
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as Hdr
import qualified System.IO as IO

import           Lib.Prelude

import           Network.Minio.XmlParser (parseErrResponse)

allocateReadFile :: (R.MonadResource m, R.MonadResourceBase m, MonadCatch m)
                 => FilePath -> m (R.ReleaseKey, Handle)
allocateReadFile fp = do
  (rk, hdlE) <- R.allocate (openReadFile fp) cleanup
  either (\(e :: IOException) -> throwM e) (return . (rk,)) hdlE
  where
    openReadFile f = ExL.try $ IO.openBinaryFile f IO.ReadMode
    cleanup = either (const $ return ()) IO.hClose

-- | Queries the file size from the handle. Catches any file operation
-- exceptions and returns Nothing instead.
getFileSize :: (R.MonadResourceBase m, R.MonadResource m)
            => Handle -> m (Maybe Int64)
getFileSize h = do
  resE <- liftIO $ try $ fromIntegral <$> IO.hFileSize h
  case resE of
    Left (_ :: IOException) -> return Nothing
    Right s -> return $ Just s

-- | Queries if handle is seekable. Catches any file operation
-- exceptions and return False instead.
isHandleSeekable :: (R.MonadResource m, R.MonadResourceBase m)
               => Handle -> m Bool
isHandleSeekable h = do
  resE <- liftIO $ try $ IO.hIsSeekable h
  case resE of
    Left (_ :: IOException) -> return False
    Right v -> return v

-- | Helper function that opens a handle to the filepath and performs
-- the given action on it. Exceptions of type MError are caught and
-- returned - both during file handle allocation and when the action
-- is run.
withNewHandle :: (R.MonadResourceBase m, R.MonadResource m, MonadCatch m)
              => FilePath -> (Handle -> m a) -> m (Either IOException a)
withNewHandle fp fileAction = do
  -- opening a handle can throw MError exception.
  handleE <- MC.try $ allocateReadFile fp
  either (return . Left) doAction handleE
  where
    doAction (rkey, h) = do
      -- fileAction may also throw MError exception, so we catch and
      -- return it.
      resE <- MC.try $ fileAction h
      R.release rkey
      return resE


lookupHeader :: HT.HeaderName -> [HT.Header] -> Maybe ByteString
lookupHeader hdr = headMay . map snd . filter (\(h, _) -> h == hdr)

getETagHeader :: [HT.Header] -> Maybe Text
getETagHeader hs = decodeUtf8Lenient <$> lookupHeader Hdr.hETag hs

getLastModifiedHeader :: [HT.Header] -> Maybe UTCTime
getLastModifiedHeader hs = do
  modTimebs <- decodeUtf8Lenient <$> lookupHeader Hdr.hLastModified hs
  parseTimeM True defaultTimeLocale rfc822DateFormat (T.unpack modTimebs)

getContentLength :: [HT.Header] -> Maybe Int64
getContentLength hs = do
  nbs <- decodeUtf8Lenient <$> lookupHeader Hdr.hContentLength hs
  fst <$> hush (decimal nbs)


decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

isSuccessStatus :: HT.Status -> Bool
isSuccessStatus sts = let s = HT.statusCode sts
                      in (s >= 200 && s < 300)

httpLbs :: (R.MonadThrow m, MonadIO m)
        => NC.Request -> NC.Manager
        -> m (NC.Response LByteString)
httpLbs req mgr = do
  respE <- liftIO $ tryHttpEx $ (NC.httpLbs req mgr)
  resp <- either throwM return respE
  unless (isSuccessStatus $ NC.responseStatus resp) $
    case contentTypeMay resp of
      Just "application/xml" -> do
        sErr <- parseErrResponse $ NC.responseBody resp
        throwM sErr

      _ -> throwM $ NC.HttpExceptionRequest req $
        NC.StatusCodeException (const () <$> resp) (show resp)

  return resp
  where
    tryHttpEx :: (IO (NC.Response LByteString))
              -> IO (Either NC.HttpException (NC.Response LByteString))
    tryHttpEx = try
    contentTypeMay resp = lookupHeader Hdr.hContentType $
                          NC.responseHeaders resp

http :: (R.MonadResourceBase m, R.MonadResource m)
     => NC.Request -> NC.Manager
     -> m (Response (C.ResumableSource m ByteString))
http req mgr = do
  respE <- tryHttpEx $ NC.http req mgr
  resp <- either throwM return respE
  unless (isSuccessStatus $ NC.responseStatus resp) $
    case contentTypeMay resp of
      Just "application/xml" -> do
        respBody <- NC.responseBody resp C.$$+- CB.sinkLbs
        sErr <- parseErrResponse $ respBody
        throwM sErr

      _ -> do
        content <- LB.toStrict . NC.responseBody <$> NC.lbsResponse resp
        throwM $ NC.HttpExceptionRequest req $
           NC.StatusCodeException (const () <$> resp) $ content


  return resp
  where
    tryHttpEx :: (R.MonadResourceBase m) => (m a)
              -> m (Either NC.HttpException a)
    tryHttpEx = ExL.try
    contentTypeMay resp = lookupHeader Hdr.hContentType $ NC.responseHeaders resp

-- like mapConcurrently but with a limited number of concurrent
-- threads.
limitedMapConcurrently :: forall t a (m :: * -> *) b.
                          (MonadIO m, R.MonadBaseControl IO m,
                           StM m a ~ StM m b)
                       => Int -> (t -> m a) -> [t] -> m [b]
limitedMapConcurrently count act args = do
  qSem <- liftIO $ Q.newQSem count
  threads <- workOn qSem args
  mapM A.wait threads
  where
    workOn _ [] = return []
    workOn qs (a:as) = liftBaseOp_
      (bracket_ (Q.waitQSem qs) (Q.signalQSem qs)) $
      do
        thread <- A.async $ act a
        others <- workOn qs as
        return (thread : others)


-- helper function to 'drop' empty optional parameter.
mkQuery :: Text -> Maybe Text -> Maybe (Text, Text)
mkQuery k mv = (k,) <$> mv

-- helper function to build query parameters that are optional.
-- don't use it with mandatory query params with empty value.
mkOptionalParams :: [(Text, Maybe Text)] -> HT.Query
mkOptionalParams params = HT.toQuery $ (uncurry  mkQuery) <$> params

chunkBSConduit :: (Monad m, Integral a)
               => [a] -> C.Conduit ByteString m ByteString
chunkBSConduit s = loop 0 [] s
  where
    loop _ _ [] = return ()
    loop n readChunks (size:sizes) = do
      bsMay <- C.await
      case bsMay of
        Nothing -> if n > 0
                   then C.yield $ B.concat readChunks
                   else return ()
        Just bs -> if n + fromIntegral (B.length bs) >= size
                   then do let (a, b) = B.splitAt (fromIntegral $ size - n) bs
                               chunkBS = B.concat $ readChunks ++ [a]
                           C.yield chunkBS
                           loop (fromIntegral $ B.length b) [b] sizes
                   else loop (n + fromIntegral (B.length bs))
                        (readChunks ++ [bs]) (size:sizes)
