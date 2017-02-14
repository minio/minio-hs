import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import qualified Data.List as L

import           Lib.Prelude

import           Network.Minio.PutObject
import           Network.Minio.XmlGenerator.Test
import           Network.Minio.XmlParser.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps] -- [scProps]

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

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "selectPartSizes: simple properties" $
    \n -> let (pns, offs, sizes) = L.unzip3 (selectPartSizes n)

              -- check that pns increments from 1.
              isPNumsAscendingFrom1 = all (\(a, b) -> a == b) $ zip pns [1..]

              consPairs [] = []
              consPairs [_] = []
              consPairs (a:(b:c)) = (a, b):(consPairs (b:c))

              -- check `offs` is monotonically increasing.
              isOffsetsAsc = all (\(a, b) -> a < b) $ consPairs offs

              -- check sizes sums to n.
              isSumSizeOk = n < 0 || (sum sizes == n && all (> 0) sizes)

              -- check sizes are constant except last
              isSizesConstantExceptLast =
                n <= 0 || all (\(a, b) -> a == b) (consPairs $ L.init sizes)

          in isPNumsAscendingFrom1 && isOffsetsAsc && isSumSizeOk &&
             isSizesConstantExceptLast

  , QC.testProperty "selectPartSizes: part-size is at least 64MiB" $
    \n -> let (_, _, sizes) = L.unzip3 (selectPartSizes n)
              mib64 = 64 * 1024 * 1024
          in if | length sizes > 1 -> -- last part can be smaller but > 0
                    all (>= mib64) (L.init sizes) && L.last sizes > 0
                | length sizes == 1 -> maybe True (> 0) $ head sizes
                | otherwise -> True
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [xmlGeneratorTests, xmlParserTests]
