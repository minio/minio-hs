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

module Network.Minio.API.Test
  ( bucketNameValidityTests
  , objectNameValidityTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Prelude

import           Network.Minio.API

assertBool' :: Bool -> Assertion
assertBool' = assertBool "Test failed!"

bucketNameValidityTests :: TestTree
bucketNameValidityTests = testGroup "Bucket Name Validity Tests"
  [ testCase "Too short 1" $ assertBool' $ not $ isValidBucketName ""
  , testCase "Too short 2" $ assertBool' $ not $ isValidBucketName "ab"
  , testCase "Too long 1" $ assertBool' $ not $ isValidBucketName "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  , testCase "Has upper case" $ assertBool' $ not $ isValidBucketName "ABCD"
  , testCase "Has punctuation" $ assertBool' $ not $ isValidBucketName "abc,2"
  , testCase "Has hyphen at end" $ assertBool' $ not $ isValidBucketName "abc-"
  , testCase "Has consecutive dot" $ assertBool' $ not $ isValidBucketName "abck..eedg"
  , testCase "Looks like IP" $  assertBool' $ not $ isValidBucketName "10.0.0.1"
  , testCase "Valid bucket name 1" $ assertBool' $ isValidBucketName "abcd.pqeq.rea"
  , testCase "Valid bucket name 2" $ assertBool' $ isValidBucketName "abcdedgh1d"
  , testCase "Valid bucket name 3" $ assertBool' $ isValidBucketName "abc-de-dg-h1d"
  ]

objectNameValidityTests :: TestTree
objectNameValidityTests = testGroup "Object Name Validity Tests"
  [ testCase "Empty name" $ assertBool' $ not $ isValidObjectName ""
  , testCase "Has unicode characters" $ assertBool' $ isValidObjectName "日本国"
  ]
