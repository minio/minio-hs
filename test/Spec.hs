import Protolude

import Test.Tasty
import Test.Tasty.HUnit

-- import qualified System.IO as SIO

import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
-- import qualified Conduit as C
-- import Data.Conduit.Binary

import Network.Minio
import Network.Minio.S3API
import Network.Minio.XmlGenerator.Test
import Network.Minio.XmlParser.Test

main :: IO ()
main = defaultMain tests
-- main = putStrLn ("Test suite not yet implemented" :: Text)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, liveServerUnitTests]

properties :: TestTree
properties = testGroup "Properties" [] -- [scProps, qcProps]

-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]

-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]

liveServerUnitTests :: TestTree
liveServerUnitTests = testGroup "Unit tests against a live server"
  [ testCaseSteps "Various functional tests" $ \step -> do

      ret <- runResourceT $ runMinio defaultConnectInfo $ do

        liftIO $ step "getService works and returns no buckets in the beginning."
        buckets <- getService
        liftIO $ (length buckets == 0) @?
          ("Live server must have no buckets at beginning.")

        liftIO $ step "putBucket works"
        putBucket "testbucket" "us-east-1"

        liftIO $ step "getLocation works"
        region <- getLocation "testbucket"
        liftIO $ region == "" @? ("Got unexpected region => " ++ show region)

        liftIO $ step "singlepart putObject works"
        fPutObject "testbucket" "lsb-release" "/etc/lsb-release"

        liftIO $ step "simple getObject works"
        fGetObject "testbucket" "lsb-release" "/tmp/out"

        liftIO $ step "create new multipart upload works"
        mp@(MultipartUpload _ _ uid) <- newMultipartUpload "testbucket"
                                        "newmpupload" []
        liftIO $ (T.length uid > 0) @?
          ("Got an empty newMultipartUpload Id => " ++ show mp)

        liftIO $ step "delete object works"
        deleteObject "testbucket" "lsb-release"

        liftIO $ step "delete bucket works"
        deleteBucket "testbucket"

      isRight ret @? ("Functional test failure => " ++ show ret)

  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Test mkCreateBucketConfig." testMkCreateBucketConfig

  , testCase "Test parseLocation." testParseLocation
  ]
