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

module Network.Minio.PutObject
  (
    putObjectInternal
  , ObjectData(..)
  , selectPartSizes
  , copyObjectInternal
  , selectCopyRanges
  , minPartSize
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
import           Network.Minio.Errors
import           Network.Minio.ListOps
import           Network.Minio.S3API
import           Network.Minio.Utils


-- | max obj size is 5TiB
maxObjectSize :: Int64
maxObjectSize = 5 * 1024 * 1024 * oneMiB

-- | minimum size of parts used in multipart operations.
minPartSize :: Int64
minPartSize = 64 * oneMiB

-- | max part of an object size is 5GiB
maxObjectPartSize :: Int64
maxObjectPartSize = 5 * 1024 * oneMiB

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
data ObjectData m =
  ODFile FilePath (Maybe Int64) -- ^ Takes filepath and optional size.
  | ODStream (C.Producer m ByteString) (Maybe Int64) -- ^ Pass size in bytes as maybe if known.

-- | Put an object from ObjectData. This high-level API handles
-- objects of all sizes, and even if the object size is unknown.
putObjectInternal :: Bucket -> Object -> ObjectData Minio -> Minio ETag
putObjectInternal b o (ODStream src sizeMay) = sequentialMultipartUpload b o sizeMay src
putObjectInternal b o (ODFile fp sizeMay) = do
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
-- be 64MiB.
selectPartSizes :: Int64 -> [(PartNumber, Int64, Int64)]
selectPartSizes size = uncurry (List.zip3 [1..]) $
                       List.unzip $ loop 0 size
  where
    ceil :: Double -> Int64
    ceil = ceiling
    partSize = max minPartSize (ceil $ fromIntegral size /
                               fromIntegral maxMultipartParts)

    m = fromIntegral partSize
    loop st sz
      | st > sz = []
      | st + m >= sz = [(st, sz - st)]
      | otherwise = (st, m) : loop (st + m) sz

-- returns partinfo if part is already uploaded.
checkUploadNeeded :: Payload -> PartNumber
                  -> Map.Map PartNumber ObjectPartInfo
                  -> Minio (Maybe PartTuple)
checkUploadNeeded payload n pmap = do
  (md5hash, pSize) <- case payload of
    PayloadBS bs -> return (hashMD5 bs, fromIntegral $ B.length bs)
    PayloadH h off size -> liftM (, size) $
      hashMD5FromSource $ sourceHandleRange h (Just $ fromIntegral off)
      (Just $ fromIntegral size)
  case Map.lookup n pmap of
    Nothing -> return Nothing
    Just (ObjectPartInfo _ etag size _) -> return $
      bool Nothing (Just (n, etag)) $
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
                  -> Minio (Maybe UploadId, Map.Map PartNumber ObjectPartInfo)
getExistingUpload b o = do
  uidMay <- (fmap . fmap) uiUploadId $
            listIncompleteUploads b (Just o) False C.$$ CC.head
  parts <- maybe (return [])
    (\uid -> listIncompleteParts b o uid C.$$ CC.sinkList) uidMay
  return (uidMay, Map.fromList $ map (\p -> (opiNumber p, p)) parts)

-- | Copy an object using single or multipart copy strategy.
copyObjectInternal :: Bucket -> Object -> CopyPartSource
                   -> Minio ETag
copyObjectInternal b' o cps = do
  -- validate and extract the src bucket and object
  (srcBucket, srcObject) <- maybe
    (throwM $ ValidationError $ MErrVInvalidSrcObjSpec $ cpSource cps)
    return $ cpsToObject cps

  -- get source object size with a head request
  (ObjectInfo _ _ _ srcSize) <- headObject srcBucket srcObject

  -- check that byte offsets are valid if specified in cps
  when (isJust (cpSourceRange cps) &&
        or [fst range < 0, snd range < fst range,
            snd range >= fromIntegral srcSize]) $
    throwM $ ValidationError $ MErrVInvalidSrcObjByteRange range

  -- 1. If sz > 64MiB (minPartSize) use multipart copy, OR
  -- 2. If startOffset /= 0 use multipart copy
  let destSize = (\(a, b) -> b - a + 1 ) $
                 maybe (0, srcSize - 1) identity $ cpSourceRange cps
      startOffset = maybe 0 fst $ cpSourceRange cps
      endOffset = maybe (srcSize - 1) snd $ cpSourceRange cps

  if destSize > minPartSize || (endOffset - startOffset + 1 /= srcSize)
    then multiPartCopyObject b' o cps srcSize

    else fst <$> copyObjectSingle b' o cps{cpSourceRange = Nothing} []

  where
    range = maybe (0, 0) identity $ cpSourceRange cps

-- | Given the input byte range of the source object, compute the
-- splits for a multipart copy object procedure. Minimum part size
-- used is minPartSize.
selectCopyRanges :: (Int64, Int64) -> [(PartNumber, (Int64, Int64))]
selectCopyRanges (st, end) = zip pns $
  map (\(x, y) -> (st + x, st + x + y - 1)) $ zip startOffsets partSizes
  where
    size = end - st + 1
    (pns, startOffsets, partSizes) = List.unzip3 $ selectPartSizes size

-- | Perform a multipart copy object action. Since we cannot verify
-- existing parts based on the source object, there is no resuming
-- copy action support.
multiPartCopyObject :: Bucket -> Object -> CopyPartSource -> Int64
                    -> Minio ETag
multiPartCopyObject b o cps srcSize = do
  uid <- newMultipartUpload b o []

  let byteRange = maybe (0, fromIntegral $ srcSize - 1) identity $
                  cpSourceRange cps
      partRanges = selectCopyRanges byteRange
      partSources = map (\(x, y) -> (x, cps {cpSourceRange = Just y}))
                    partRanges

  copiedParts <- limitedMapConcurrently 10
                 (\(pn, cps') -> do
                     (etag, _) <- copyObjectPart b o cps' uid pn []
                     return $ (pn, etag)
                 )
                 partSources

  completeMultipartUpload b o uid copiedParts
