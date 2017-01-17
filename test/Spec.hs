import Protolude

import Test.Tasty
import Test.Tasty.HUnit

-- import qualified System.IO as SIO

import Control.Monad.Trans.Resource (runResourceT)

-- import qualified Conduit as C
-- import Data.Conduit.Binary

import Network.Minio
-- import Network.Minio.S3API
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
  [ testCase "Check getService returns without exception" $ do
      ret <- runResourceT $ runMinio defaultConnectInfo $ getService
      isRight ret @? ("getService failure => " ++ show ret)

  , testCase "Simple fGetObject works" $ do
      ret <- runResourceT $ runMinio defaultConnectInfo $
             fGetObject "testbucket" "lsb-release" "/tmp/out"
      isRight ret @? ("fGetObject failure => " ++ show ret)

  , testCase "Simple putObject works" $ do
      ret <- runResourceT $ runMinio defaultConnectInfo $
             fPutObject "testbucket" "lsb-release" "/etc/lsb-release"
      isRight ret @? ("putObject failure => " ++ show ret)

  , testCase "Simple putObject fails with non-existent file" $ do
      ret <- runResourceT $ runMinio defaultConnectInfo $
             fPutObject "testbucket" "lsb-release" "/etc/lsb-releaseXXX"
      isLeft ret @? ("putObject unexpected success => " ++ show ret)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Test mkCreateBucketConfig." testMkCreateBucketConfig

  , testCase "Test parseLocation." testParseLocation
  ]
