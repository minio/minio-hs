module Network.Minio.XmlParser.Test
  (
    xmlParserTests
  ) where

import qualified Control.Monad.Catch as MC
import           Data.Time (fromGregorian, UTCTime(..))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.XmlParser

xmlParserTests :: TestTree
xmlParserTests = testGroup "XML Parser Tests"
  [ testCase "Test parseLocation" testParseLocation
  , testCase "Test parseNewMultipartUpload" testParseNewMultipartUpload
  , testCase "Test parseListObjectsResponse" testParseListObjectsResult
  , testCase "Test parseListUploadsresponse" testParseListIncompleteUploads
  , testCase "Test parseCompleteMultipartUploadResponse" testParseCompleteMultipartUploadResponse
  , testCase "Test parseListPartsResponse" testParseListPartsResponse
  ]

tryMError :: (MC.MonadCatch m) => m a -> m (Either MError a)
tryMError act = MC.try act

assertMError :: MError -> Assertion
assertMError e = assertFailure $ "Failed due to exception => " ++ show e

eitherMError :: Either MError a -> (a -> Assertion) -> Assertion
eitherMError (Left e) _ = assertMError e
eitherMError (Right a) f = f a

testParseLocation :: Assertion
testParseLocation = do
  -- 1. Test parsing of an invalid location constraint xml.
  parseResE <- tryMError $ parseLocation "ClearlyInvalidXml"
  when (isRight parseResE) $
    assertFailure $ "Parsing should have failed => " ++ show parseResE

  forM_ cases $ \(xmldata, expectedLocation) -> do
    parseLocE <- tryMError $ parseLocation xmldata
    either assertMError (@?= expectedLocation) parseLocE
  where
    cases =  [
      -- 2. Test parsing of a valid location xml.
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
       \<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">EU</LocationConstraint>",
       "EU"
      )
      ,
      -- 3. Test parsing of a valid, empty location xml.
      ("<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"/>",
       ""
      )
      ]


testParseNewMultipartUpload :: Assertion
testParseNewMultipartUpload = do
  forM_ cases $ \(xmldata, expectedUploadId) -> do
    parsedUploadIdE <- tryMError $ parseNewMultipartUpload xmldata
    eitherMError parsedUploadIdE (@?= expectedUploadId)
  where
    cases = [
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
       \<InitiateMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
       \  <Bucket>example-bucket</Bucket>\
       \  <Key>example-object</Key>\
       \  <UploadId>VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA</UploadId>\
       \</InitiateMultipartUploadResult>",
       "VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA"
      ),
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
       \<InitiateMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
       \  <Bucket>example-bucket</Bucket>\
       \  <Key>example-object</Key>\
       \  <UploadId>EXAMPLEJZ6e0YupT2h66iePQCc9IEbYbDUy4RTpMeoSMLPRp8Z5o1u8feSRonpvnWsKKG35tI2LB9VDPiCgTy.Gq2VxQLYjrue4Nq.NBdqI-</UploadId>\
       \</InitiateMultipartUploadResult>",
       "EXAMPLEJZ6e0YupT2h66iePQCc9IEbYbDUy4RTpMeoSMLPRp8Z5o1u8feSRonpvnWsKKG35tI2LB9VDPiCgTy.Gq2VxQLYjrue4Nq.NBdqI-"
      )
      ]

testParseListObjectsResult :: Assertion
testParseListObjectsResult = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
              \<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
              \<Name>bucket</Name>\
              \<Prefix/>\
              \<KeyCount>205</KeyCount>\
              \<MaxKeys>1000</MaxKeys>\
              \<IsTruncated>false</IsTruncated>\
              \<Contents>\
              \<Key>my-image.jpg</Key>\
              \<LastModified>2009-10-12T17:50:30.000Z</LastModified>\
              \<ETag>&quot;fba9dede5f27731c9771645a39863328&quot;</ETag>\
              \<Size>434234</Size>\
              \<StorageClass>STANDARD</StorageClass>\
              \</Contents>\
              \</ListBucketResult>"

    expectedListResult = ListObjectsResult False Nothing [object1] []
    object1 = ObjectInfo "my-image.jpg" modifiedTime1 "\"fba9dede5f27731c9771645a39863328\"" 434234
    modifiedTime1 = flip UTCTime 64230 $ fromGregorian 2009 10 12

  parsedListObjectsResult <- tryMError $ parseListObjectsResponse xmldata
  eitherMError parsedListObjectsResult (@?= expectedListResult)

testParseListIncompleteUploads :: Assertion
testParseListIncompleteUploads = do
  let
    xmldata = "<ListMultipartUploadsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
  \<Bucket>example-bucket</Bucket>\
  \<KeyMarker/>\
  \<UploadIdMarker/>\
  \<NextKeyMarker>sample.jpg</NextKeyMarker>\
  \<NextUploadIdMarker>Xgw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1W99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--</NextUploadIdMarker>\
  \<Delimiter>/</Delimiter>\
  \<Prefix/>\
  \<MaxUploads>1000</MaxUploads>\
  \<IsTruncated>false</IsTruncated>\
  \<Upload>\
    \<Key>sample.jpg</Key>\
    \<UploadId>Agw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1N99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--</UploadId>\
    \<Initiator>\
      \<ID>314133b66967d86f031c7249d1d9a80249109428335cd0ef1cdc487b4566cb1b</ID>\
      \<DisplayName>s3-nickname</DisplayName>\
    \</Initiator>\
    \<Owner>\
      \<ID>314133b66967d86f031c7249d1d9a80249109428335cd0ef1cdc487b4566cb1b</ID>\
      \<DisplayName>s3-nickname</DisplayName>\
    \</Owner>\
    \<StorageClass>STANDARD</StorageClass>\
    \<Initiated>2010-11-26T19:24:17.000Z</Initiated>\
  \</Upload>\
  \<CommonPrefixes>\
    \<Prefix>photos/</Prefix>\
  \</CommonPrefixes>\
  \<CommonPrefixes>\
    \<Prefix>videos/</Prefix>\
  \</CommonPrefixes>\
  \</ListMultipartUploadsResult>"
    expectedListResult = ListUploadsResult False (Just "sample.jpg") (Just "Xgw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1W99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--") uploads prefixes
    uploads = [UploadInfo "sample.jpg" "Agw4MJT6ZPAVxpY0SAuGN7q4uWJJM22ZYg1N99trdp4tpO88.PT6.MhO0w2E17eutfAvQfQWoajgE_W2gpcxQw--" initTime]
    initTime = UTCTime (fromGregorian 2010 11 26) 69857
    prefixes = ["photos/", "videos/"]

  parsedListUploadsResult <- tryMError $ parseListUploadsResponse xmldata
  eitherMError parsedListUploadsResult (@?= expectedListResult)


testParseCompleteMultipartUploadResponse :: Assertion
testParseCompleteMultipartUploadResponse = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<CompleteMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
  \<Location>http://Example-Bucket.s3.amazonaws.com/Example-Object</Location>\
  \<Bucket>Example-Bucket</Bucket>\
  \<Key>Example-Object</Key>\
  \<ETag>\"3858f62230ac3c915f300c664312c11f-9\"</ETag>\
\</CompleteMultipartUploadResult>"
    expectedETag = "\"3858f62230ac3c915f300c664312c11f-9\""

  parsedETagE <- runExceptT $ parseCompleteMultipartUploadResponse xmldata
  eitherMError parsedETagE (@?= expectedETag)

testParseListPartsResponse :: Assertion
testParseListPartsResponse = do
  let
    xmldata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<ListPartsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
  \<Bucket>example-bucket</Bucket>\
  \<Key>example-object</Key>\
  \<UploadId>XXBsb2FkIElEIGZvciBlbHZpbmcncyVcdS1tb3ZpZS5tMnRzEEEwbG9hZA</UploadId>\
  \<Initiator>\
      \<ID>arn:aws:iam::111122223333:user/some-user-11116a31-17b5-4fb7-9df5-b288870f11xx</ID>\
      \<DisplayName>umat-user-11116a31-17b5-4fb7-9df5-b288870f11xx</DisplayName>\
  \</Initiator>\
  \<Owner>\
    \<ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>\
    \<DisplayName>someName</DisplayName>\
  \</Owner>\
  \<StorageClass>STANDARD</StorageClass>\
  \<PartNumberMarker>1</PartNumberMarker>\
  \<NextPartNumberMarker>3</NextPartNumberMarker>\
  \<MaxParts>2</MaxParts>\
  \<IsTruncated>true</IsTruncated>\
  \<Part>\
    \<PartNumber>2</PartNumber>\
    \<LastModified>2010-11-10T20:48:34.000Z</LastModified>\
    \<ETag>\"7778aef83f66abc1fa1e8477f296d394\"</ETag>\
    \<Size>10485760</Size>\
  \</Part>\
  \<Part>\
    \<PartNumber>3</PartNumber>\
    \<LastModified>2010-11-10T20:48:33.000Z</LastModified>\
    \<ETag>\"aaaa18db4cc2f85cedef654fccc4a4x8\"</ETag>\
    \<Size>10485760</Size>\
  \</Part>\
\</ListPartsResult>"

    expectedListResult = ListPartsResult True (Just 3) [part1, part2]
    part1 = ListPartInfo 2 "\"7778aef83f66abc1fa1e8477f296d394\"" 10485760 modifiedTime1
    modifiedTime1 = flip UTCTime 74914 $ fromGregorian 2010 11 10
    part2 = ListPartInfo 3 "\"aaaa18db4cc2f85cedef654fccc4a4x8\"" 10485760 modifiedTime2
    modifiedTime2 = flip UTCTime 74913 $ fromGregorian 2010 11 10

  parsedListPartsResult <- runExceptT $ parseListPartsResponse xmldata
  eitherMError parsedListPartsResult (@?= expectedListResult)
