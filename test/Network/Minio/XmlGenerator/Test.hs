module Network.Minio.XmlGenerator.Test
  ( xmlGeneratorTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Lib.Prelude

import Network.Minio.XmlGenerator
import Network.Minio.Data

xmlGeneratorTests :: TestTree
xmlGeneratorTests = testGroup "XML Generator Tests"
  [ testCase "Test mkCreateBucketConfig" testMkCreateBucketConfig
  , testCase "Test mkCompleteMultipartUploadRequest" testMkCompleteMultipartUploadRequest
  ]

testMkCreateBucketConfig :: Assertion
testMkCreateBucketConfig = do
  assertEqual "CreateBucketConfiguration xml should match: " expected $
    mkCreateBucketConfig "EU"
  where
    expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><LocationConstraint>EU</LocationConstraint></CreateBucketConfiguration>"

testMkCompleteMultipartUploadRequest :: Assertion
testMkCompleteMultipartUploadRequest =
  assertEqual "completeMultipartUpload xml should match: " expected $
  mkCompleteMultipartUploadRequest [PartInfo 1 "abc"]
  where
    expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><CompleteMultipartUpload><Part><PartNumber>1</PartNumber><ETag>abc</ETag></Part></CompleteMultipartUpload>"
