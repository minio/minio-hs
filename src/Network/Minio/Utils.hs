module Network.Minio.Utils where

import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.QSem as Q
import qualified Control.Exception.Lifted as ExL
import           Control.Monad.Trans.Control (liftBaseOp_, StM)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Network.HTTP.Client as NClient
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Types as HT
import qualified System.IO as IO

import           Lib.Prelude

import           Network.Minio.Data

allocateReadFile :: (R.MonadResource m, R.MonadResourceBase m)
                 => FilePath -> m (R.ReleaseKey, Handle)
allocateReadFile fp = do
  (rk, hdlE) <- R.allocate (openReadFile fp) cleanup
  either (throwM . MEFile) (return . (rk,)) hdlE
  where
    openReadFile f = ExL.try $ IO.openBinaryFile f IO.ReadMode
    cleanup = either (const $ return ()) IO.hClose

getFileSize :: (R.MonadResourceBase m, R.MonadResource m)
            => Handle -> m (Either IOException Int64)
getFileSize h = ExL.try $ liftIO $ fromIntegral <$> IO.hFileSize h

isFileSeekable :: (R.MonadResource m, R.MonadResourceBase m)
               => FilePath -> m Bool
isFileSeekable fp = do
  (rKey, h) <- allocateReadFile fp
  resE <- liftIO $ try $ IO.hIsSeekable h
  R.release rKey
  either (throwM . MEFile) return resE


lookupHeader :: HT.HeaderName -> [HT.Header] -> Maybe ByteString
lookupHeader hdr = headMay . map snd . filter (\(h, _) -> h == hdr)

getETagHeader :: [HT.Header] -> Maybe Text
getETagHeader hs = decodeUtf8Lenient <$> lookupHeader "ETag" hs

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

isSuccessStatus :: HT.Status -> Bool
isSuccessStatus sts = let s = HT.statusCode sts
                      in (s >= 200 && s < 300)

httpLbs :: (R.MonadThrow m, MonadIO m)
        => NC.Request -> NC.Manager
        -> m (NC.Response LByteString)
httpLbs req mgr = do
  respE <- liftIO $ tryHttpEx $ (NClient.httpLbs req mgr)
  resp <- either throwM return respE
  unless (isSuccessStatus $ NC.responseStatus resp) $
    throwM $ ResponseError resp
  return resp
  where
    tryHttpEx :: (IO (NC.Response LByteString))
              -> IO (Either NC.HttpException (NC.Response LByteString))
    tryHttpEx = try

http :: (R.MonadResourceBase m, R.MonadResource m)
     => NC.Request -> NC.Manager
     -> m (Response (C.ResumableSource m ByteString))
http req mgr = do
  respE <- tryHttpEx $ NC.http req mgr
  resp <- either throwM return respE
  unless (isSuccessStatus $ NC.responseStatus resp) $ do
    lbsResp <- NC.lbsResponse resp
    throwM $ ResponseError lbsResp
  return resp
  where
    tryHttpEx :: (R.MonadResourceBase m) => (m a)
              -> m (Either NC.HttpException a)
    tryHttpEx = ExL.try

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
