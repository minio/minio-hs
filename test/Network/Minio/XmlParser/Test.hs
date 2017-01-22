module Network.Minio.XmlParser.Test
  (
    xmlParserTests
  ) where

import Data.Time (addGregorianYearsClip, fromGregorian, UTCTime(..))
import Test.Tasty
import Test.Tasty.HUnit

import Lib.Prelude

import Network.Minio.Data
import Network.Minio.XmlParser

xmlParserTests :: TestTree
xmlParserTests = testGroup "XML Parser Tests"
  [ testCase "Test parseLocation" testParseLocation
  , testCase "Test parseNewMultipartUpload" testParseNewMultipartUpload
  , testCase "Test parseListObjectsResponse" testParseListObjectsResult
  ]

testParseLocation :: Assertion
testParseLocation = do
  -- 1. Test parsing of an invalid location constraint xml.
  parsedLocationE <- runExceptT $ parseLocation "ClearlyInvalidXml"
  case parsedLocationE of
    Right _ -> assertFailure $ "Parsing should have failed => " ++ show parsedLocationE
    Left _ -> return ()

  forM_ cases $ \(xmldata, expectedLocation) -> do
    parsedLocationE1 <- runExceptT $ parseLocation xmldata
    case parsedLocationE1 of
      Right parsedLocation -> parsedLocation @?= expectedLocation
      _ -> assertFailure $ "Parsing failed => " ++ show parsedLocationE1
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
    parsedUploadIdE <- runExceptT $ parseNewMultipartUpload xmldata
    case parsedUploadIdE of
      Right upId -> upId @?= expectedUploadId
      _ -> assertFailure $ "Parsing failed => " ++ show parsedUploadIdE
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

    parsedListObjectsResult <- runExceptT $ parseListObjectsResponse xmldata
    case parsedListObjectsResult of
        Right listObjectsResult -> listObjectsResult @?= expectedListResult
        _ -> assertFailure $ "Parsing failed => " ++ show parsedListObjectsResult
