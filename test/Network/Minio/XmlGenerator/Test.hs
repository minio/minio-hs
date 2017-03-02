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

module Network.Minio.XmlGenerator.Test
  ( xmlGeneratorTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Lib.Prelude

import Network.Minio.XmlGenerator

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
    expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
               \<CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
               \<LocationConstraint>EU</LocationConstraint>\
               \</CreateBucketConfiguration>"

testMkCompleteMultipartUploadRequest :: Assertion
testMkCompleteMultipartUploadRequest =
  assertEqual "completeMultipartUpload xml should match: " expected $
  mkCompleteMultipartUploadRequest [(1, "abc")]
  where
    expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<CompleteMultipartUpload>\
    \<Part>\
    \<PartNumber>1</PartNumber><ETag>abc</ETag>\
    \</Part>\
    \</CompleteMultipartUpload>"
