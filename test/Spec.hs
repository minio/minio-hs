import Protolude

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Resource (runResourceT)

import Network.Minio
import XmlTests

main :: IO ()
main = defaultMain tests
-- main = putStrLn ("Test suite not yet implemented" :: Text)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

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

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCaseSteps "Check getService returns without exception" $ \step -> do
      step "Preparing..."

      mc <- connect defaultConnectInfo

      step "Running test.."
      ret <- runResourceT $ runMinio mc $ getService
      isRight ret @? ("getService failure => " ++ show ret)

  , testCase "Test mkCreateBucketConfig." testMkCreateBucketConfig
  ]
