module Network.Minio.PutObject
  (
    putObject
  , ObjectData(..)
  , selectPartSizes
  ) where


import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Data.Conduit.Binary (sourceHandleRange)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.ListOps
import           Network.Minio.S3API
import           Network.Minio.Utils


-- | max obj size is 5TiB
maxObjectSize :: Int64
maxObjectSize = 5 * 1024 * 1024 * oneMiB

oneMiB :: Int64
oneMiB = 1024 * 1024

maxMultipartParts :: Int64
maxMultipartParts = 10000

-- | A data-type to represent the source data for an object. A
-- file-path or a producer-conduit may be provided.
--
-- For files, a size may be provided - this is useful in cases when
-- the file size cannot be automatically determined or if only some
-- prefix of the file is desired.
--
-- For streams also, a size may be provided. This is useful to limit
-- the input - if it is not provided, upload will continue until the
-- stream ends or the object reaches `maxObjectsize` size.
data ObjectData m = ODFile FilePath (Maybe Int64) -- ^ Takes filepath and optional size.
                  | ODStream (C.Producer m ByteString) (Maybe Int64) -- ^ Pass size in bytes as maybe if known.

-- | Put an object from ObjectData. This high-level API handles
-- objects of all sizes, and even if the object size is unknown.
putObject :: Bucket -> Object -> ObjectData Minio -> Minio ETag
putObject b o (ODStream src sizeMay) = sequentialMultipartUpload b o sizeMay src
putObject b o (ODFile fp sizeMay) = do
  hResE <- withNewHandle fp $ \h ->
    liftM2 (,) (isHandleSeekable h) (getFileSize h)

  (isSeekable, handleSizeMay) <- either (const $ return (False, Nothing)) return
                                 hResE

  -- prefer given size to queried size.
  let finalSizeMay = listToMaybe $ catMaybes [sizeMay, handleSizeMay]

  case finalSizeMay of
    -- unable to get size, so assume non-seekable file and max-object size
    Nothing -> sequentialMultipartUpload b o (Just maxObjectSize) $
               CB.sourceFile fp

    -- got file size, so check for single/multipart upload
    Just size ->
      if | size <= 64 * oneMiB -> either throwM return =<<
           withNewHandle fp (\h -> putObjectSingle b o [] h 0 size)
         | size > maxObjectSize -> throwM $ ValidationError $
                                   MErrVPutSizeExceeded size
         | isSeekable -> parallelMultipartUpload b o fp size
         | otherwise -> sequentialMultipartUpload b o (Just size) $
                        CB.sourceFile fp

-- | Select part sizes - the logic is that the minimum part-size will
-- be 64MiB. TODO: write quickcheck tests.
selectPartSizes :: Int64 -> [(PartNumber, Int64, Int64)]
selectPartSizes size = List.zip3 [1..] partOffsets partSizes
  where
    ceil :: Double -> Int64
    ceil = ceiling
    partSize = max (64 * oneMiB) (ceil $ fromIntegral size /
                                  fromIntegral maxMultipartParts)
    (numParts, lastPartSize) = size `divMod` partSize
    lastPart = filter (> 0) [lastPartSize]
    partSizes = replicate (fromIntegral numParts) partSize ++ lastPart
    partOffsets = List.scanl' (+) 0 partSizes

-- returns partinfo if part is already uploaded.
checkUploadNeeded :: Payload -> PartNumber
                  -> Map.Map PartNumber ListPartInfo
                  -> Minio (Maybe PartInfo)
checkUploadNeeded payload n pmap = do
  (md5hash, pSize) <- case payload of
    PayloadBS bs -> return (hashMD5 bs, fromIntegral $ B.length bs)
    PayloadH h off size -> liftM (, size) $
      hashMD5FromSource $ sourceHandleRange h (Just $ fromIntegral off)
      (Just $ fromIntegral size)
  case Map.lookup n pmap of
    Nothing -> return Nothing
    Just (ListPartInfo _ etag size _) -> return $
      bool Nothing (Just (PartInfo n etag)) $
      md5hash == encodeUtf8 etag && size == pSize

parallelMultipartUpload :: Bucket -> Object -> FilePath -> Int64
                        -> Minio ETag
parallelMultipartUpload b o filePath size = do
  (uidMay, pmap) <- getExistingUpload b o

  -- get a new upload id if needed.
  uploadId <- maybe (newMultipartUpload b o []) return uidMay

  let partSizeInfo = selectPartSizes size

  -- perform upload with 10 threads
  uploadedPartsE <- limitedMapConcurrently 10
                    (uploadPart pmap uploadId) partSizeInfo

  -- if there were any errors, rethrow exception.
  mapM_ throwM $ lefts uploadedPartsE

  -- if we get here, all parts were successfully uploaded.
  completeMultipartUpload b o uploadId $ rights uploadedPartsE
  where
    uploadPart pmap uploadId (partNum, offset, sz) =
      withNewHandle filePath $ \h -> do
        let payload = PayloadH h offset sz
        pInfoMay <- checkUploadNeeded payload partNum pmap
        maybe
          (putObjectPart b o uploadId partNum [] payload)
          return pInfoMay

-- | Upload multipart object from conduit source sequentially
sequentialMultipartUpload :: Bucket -> Object -> Maybe Int64
                          -> C.Producer Minio ByteString -> Minio ETag
sequentialMultipartUpload b o sizeMay src = do
  (uidMay, pmap) <- getExistingUpload b o

  -- get a new upload id if needed.
  uploadId <- maybe (newMultipartUpload b o []) return uidMay

  -- upload parts in loop
  let partSizes = selectPartSizes $ maybe maxObjectSize identity sizeMay
      (pnums, _, sizes) = List.unzip3 partSizes
  uploadedParts <- src
              C..| chunkBSConduit sizes
              C..| CL.map PayloadBS
              C..| checkAndUpload uploadId pmap pnums
              C.$$ CC.sinkList

  -- complete multipart upload
  completeMultipartUpload b o uploadId uploadedParts

  where
    checkAndUpload _ _ [] = return ()
    checkAndUpload uid pmap (pn:pns) = do
      payloadMay <- C.await
      case payloadMay of
        Nothing -> return ()
        Just payload -> do partMay <- lift $ checkUploadNeeded payload pn pmap
                           pinfo <- maybe
                                    (lift $ putObjectPart b o uid pn [] payload)
                                    return partMay
                           C.yield pinfo
                           checkAndUpload uid pmap pns

-- | Looks for incomplete uploads for an object. Returns the first one
-- if there are many.
getExistingUpload :: Bucket -> Object
                  -> Minio (Maybe UploadId, Map.Map PartNumber ListPartInfo)
getExistingUpload b o = do
  uidMay <- (fmap . fmap) uiUploadId $
            listIncompleteUploads b (Just o) False C.$$ CC.head
  parts <- maybe (return [])
    (\uid -> listIncompleteParts b o uid C.$$ CC.sinkList) uidMay
  return (uidMay, Map.fromList $ map (\p -> (piNumber p, p)) parts)
