module Network.Minio.XmlParser.Test
  (
    xmlParserTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Lib.Prelude

-- import Network.Minio.Data
import Network.Minio.XmlParser

xmlParserTests :: TestTree
xmlParserTests = testGroup "XML Parser Tests"
  [ testCase "Test parseLocation" testParseLocation
  , testCase "Test parseNewMultipartUpload" testParseNewMultipartUpload
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
