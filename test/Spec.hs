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
  [ QC.testProperty "selectPartSizes:" $
    \n -> let (pns, offs, sizes) = L.unzip3 (selectPartSizes n)

              -- check that pns increments from 1.
              isPNumsAscendingFrom1 = all (\(a, b) -> a == b) $ zip pns [1..]

              consPairs [] = []
              consPairs [_] = []
              consPairs (a:(b:c)) = (a, b):(consPairs (b:c))

              -- check `offs` is monotonically increasing.
              isOffsetsAsc = all (\(a, b) -> a < b) $ consPairs offs

              -- check sizes sums to n.
              isSumSizeOk = sum sizes == n

              -- check sizes are constant except last
              isSizesConstantExceptLast =
                all (\(a, b) -> a == b) (consPairs $ L.init sizes)

              -- check each part except last is at least minPartSize;
              -- last part may be 0 only if it is the only part.
              nparts = length sizes
              isMinPartSizeOk =
                if | nparts > 1 -> -- last part can be smaller but > 0
                     all (>= minPartSize) (take (nparts - 1) sizes) &&
                     all (\s -> s > 0) (drop (nparts - 1) sizes)
                   | nparts == 1 -> -- size may be 0 here.
                       maybe True (\x -> x >= 0 && x <= minPartSize) $
                       headMay sizes
                   | otherwise -> False

          in n < 0 ||
             (isPNumsAscendingFrom1 && isOffsetsAsc && isSumSizeOk &&
              isSizesConstantExceptLast && isMinPartSizeOk)

  , QC.testProperty "selectCopyRanges:" $
    \(start, end) ->
      let (_, pairs) = L.unzip (selectCopyRanges (start, end))

          -- is last part's snd offset end?
          isLastPartOk = maybe False ((end ==) . snd) $ lastMay pairs
          -- is first part's fst offset start
          isFirstPartOk = maybe False ((start ==) . fst) $ headMay pairs

          -- each pair is >=64MiB except last, and all those parts
          -- have same size.
          initSizes = maybe [] (map (\(a, b) -> b - a + 1)) $ initMay pairs
          isPartSizesOk = all (>= minPartSize) initSizes &&
                          maybe True (\k -> all (== k) initSizes)
                          (headMay initSizes)

          -- returned offsets are contiguous.
          fsts = drop 1 $ map fst pairs
          snds = take (length pairs - 1) $ map snd pairs
          isContParts = length fsts == length snds &&
                        and (map (\(a, b) -> a == b + 1) $ zip fsts snds)

      in start < 0 || start > end ||
         (isLastPartOk && isFirstPartOk && isPartSizesOk && isContParts)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [xmlGeneratorTests, xmlParserTests]
