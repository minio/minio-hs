module Network.Minio.PutObject
  (
    putObject
  , ObjectData(..)
  ) where


import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.List as List
import qualified Data.ByteString.Lazy as LB
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
      if | size <= 64 * oneMiB -> do
             resE <- withNewHandle fp (\h -> putObjectSingle b o [] h 0 size)
             either throwM return resE
         | size > maxObjectSize -> throwM $ ValidationError $
                                   MErrVPutSizeExceeded size
         | isSeekable -> parallelMultipartUpload b o fp size
         | otherwise -> sequentialMultipartUpload b o (Just size) $
                        CB.sourceFile fp
putObject b o (ODStream src sizeMay) = sequentialMultipartUpload b o sizeMay src

-- | Select part sizes - the logic is that the minimum part-size will
-- be 64MiB. TODO: write quickcheck tests.
selectPartSizes :: Int64 -> [(PartNumber, Int64, Int64)]
selectPartSizes size = List.zip3 [1..] partOffsets partSizes
  where
    partSize = max (64 * oneMiB) (size `div` maxMultipartParts)
    (numParts, lastPartSize) = size `divMod` partSize
    lastPart = filter (> 0) [lastPartSize]
    partSizes = replicate (fromIntegral numParts) partSize ++ lastPart
    partOffsets = List.scanl' (+) 0 partSizes

parallelMultipartUpload :: Bucket -> Object -> FilePath -> Int64
                        -> Minio ETag
parallelMultipartUpload b o filePath size = do
  let partSizeInfo = selectPartSizes size

  -- get new upload id.
  uploadId <- newMultipartUpload b o []

  -- perform upload with 10 threads
  uploadedPartsE <- limitedMapConcurrently 10 (uploadPart uploadId) partSizeInfo

  -- if there were any errors, rethrow exception.
  mapM_ throwM $ lefts uploadedPartsE

  -- if we get here, all parts were successfully uploaded.
  completeMultipartUpload b o uploadId $ rights uploadedPartsE
  where
    uploadPart uploadId (partNum, offset, sz) = withNewHandle filePath $
      \h -> putObjectPart b o uploadId partNum [] $ PayloadH h offset sz

-- | Upload multipart object from conduit source sequentially
sequentialMultipartUpload :: Bucket -> Object -> Maybe Int64
                          -> C.Producer Minio ByteString -> Minio ETag
sequentialMultipartUpload b o sizeMay src = do
  (uidMay, pinfos) <- getExistingUpload b o

  -- get a new upload id if needed.
  uploadId <- maybe (newMultipartUpload b o []) return uidMay

  -- upload parts in loop
  uploadedParts <- loop pinfos uploadId rSrc partSizeInfo []

  -- complete multipart upload
  completeMultipartUpload b o uploadId uploadedParts
  where
    rSrc = C.newResumableSource src
    partSizeInfo = selectPartSizes $ maybe maxObjectSize identity sizeMay

    -- returns partinfo if part is already uploaded.
    checkUploadNeeded :: LByteString -> PartNumber
                      -> Map.Map PartNumber ListPartInfo
                      -> Maybe PartInfo
    checkUploadNeeded lbs n pmap = do
      pinfo@(ListPartInfo _ etag size _) <- Map.lookup n pmap
      bool Nothing (return (PartInfo n etag)) $
        LB.length lbs == size &&
        hashMD5 (LB.toStrict lbs) == encodeUtf8 etag

    -- make a sink that consumes only `s` bytes
    limitedSink s = CB.isolate (fromIntegral s) C.=$= CB.sinkLbs

    -- FIXME: test, confirm and remove traceShowM statements
    loop _ _ _ [] uparts = return $ reverse uparts
    loop pinfos uid rSource ((partNum, _, size):ps) u = do

      -- load data from resume-able source into bytestring.
      (newSource, buf) <- rSource C.$$++ (limitedSink size)
      traceShowM "psize: "
      traceShowM (LB.length buf)

      case checkUploadNeeded buf partNum pinfos of
        Just pinfo -> loop pinfos uid newSource ps (pinfo:u)
        Nothing -> do
          pInfo <- putObjectPart b o uid partNum [] $
                   PayloadBS $ LB.toStrict buf

          if LB.length buf == size
               -- upload the full size part.
               then loop pinfos uid newSource ps (pInfo:u)

               -- got a smaller part, so its the last one.
               else do traceShowM (("Found a piece with length < than "::[Char]) ++ show size ++ " - uploading as last and quitting.")
                       finalData <- newSource C.$$+- (limitedSink size)
                       traceShowM "finalData size:"
                       traceShowM (LB.length finalData)
                       return $ reverse (pInfo:u)

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
