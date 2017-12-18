module Main (main)
  where
import qualified Data.Vector.Storable             as V
import           Foreign.C.Types
import           OwenCDF1
import           OwenCDF2
import           OwenCDF3
import           OwenCDF4
import           OwenQ1
import           OwenQ2
import           OwenT
import           Statistics.Distribution
import           Statistics.Distribution.StudentT
import           Student
import           Test.Tasty                       (defaultMain, testGroup)
import           Test.Tasty.HUnit                 (testCase)
import           Test.Tasty.HUnit                 (Assertion, assertEqual,
                                                   (@=?))

studentCDF' :: CDouble -> CInt -> CDouble -> IO CDouble
studentCDF' q nu delta = do
  value <- studentCDF q nu [delta]
  return $ value V.! 0

owenQ1' :: CInt -> CDouble -> CDouble -> CDouble -> IO CDouble
owenQ1' nu t delta r = do
  value <- owenQ1 nu t [delta] [r]
  return $ value V.! 0

owenQ2' :: CInt -> CDouble -> CDouble -> CDouble -> IO CDouble
owenQ2' nu t delta r = do
  value <- owenQ2 nu t [delta] [r]
  return $ value V.! 0

owenCDF4' :: CInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble
owenCDF4' nu t1 t2 delta1 delta2 = do
  value <- owenCDF4 nu t1 t2 [delta1] [delta2]
  return $ value V.! 0

owenCDF4_ :: CInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble
owenCDF4_ nu t1 t2 delta1 delta2 = do
  value1 <- owenQ1 nu t2 [delta2] [r]
  value2 <- owenQ1 nu t1 [delta1] [r]
  return $ (value1 V.! 0) - (value2 V.! 0)
  where r = sqrt(fromIntegral nu) * (delta1-delta2) / (t1-t2)

owenCDF1' :: CInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble
owenCDF1' nu t1 t2 delta1 delta2 = do
  value <- owenCDF1 nu t1 t2 [delta1] [delta2]
  return $ value V.! 0

owenCDF2' :: CInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble
owenCDF2' nu t1 t2 delta1 delta2 = do
  value <- owenCDF2 nu t1 t2 [delta1] [delta2]
  return $ value V.! 0

owenCDF3' :: CInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble
owenCDF3' nu t1 t2 delta1 delta2 = do
  value <- owenCDF3 nu t1 t2 [delta1] [delta2]
  return $ value V.! 0

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [
    testGroup "OwenCDF4"
    [
      testCase "OwenCDF4 value 1" $ do
        x <- owenCDF4' 6 2 1 3 2
        (@~?) x 0.01785518085912236 9,
      testCase "OwenCDF4' value 1" $ do
        x <- owenCDF4_ 6 2 1 3 2
        (@~?) x 0.01785518085912236 9,
      testCase "OwenCDF4 value 2" $ do
        x <- owenCDF4' 5 2 1 3 2
        (@~?) x 0.01868982415809893 8,
      testCase "OwenCDF4' value 2" $ do
        x <- owenCDF4_ 5 2 1 3 2
        (@~?) x 0.01868982415809893 8,
      testCase "OwenCDF4 delta1=Inf - independent of t1" $ do
        x1 <- owenCDF4' 2 3 1 100 2
        x2 <- owenCDF4' 2 13 1 (1/0) 2
        (@~?) x1 x2 11,
      testCase "OwenCDF4 delta1=Inf - equal studentCDF" $ do
        x1 <- owenCDF4' 2 3 1 (1/0) 2
        x2 <- studentCDF' 1 2 2
        (@~?) x1 x2 11
    ],
    testGroup "OwenQ1"
    [
      testCase "OwenQ1 for large R = studentCDF" $ do
        x1 <- owenQ1' 4 3 2 100
        x2 <- studentCDF' 3 4 2
        (@~?) x1 x2 11
    ],
    testGroup "OwenQ2"
    [
      testCase "OwenQ1 + OwenQ2 = studentCDF - test1" $ do
        o1 <- owenQ1' 6 2 2 1
        o2 <- owenQ2' 6 2 2 1
        s <- studentCDF' 2 6 2
        (@~?) (o1+o2) s 14,
      testCase "OwenQ1 + OwenQ2 = studentCDF - test2" $ do
        o1 <- owenQ1' 7 2 2 1
        o2 <- owenQ2' 7 2 2 1
        s <- studentCDF' 2 7 2
        (@~?) (o1+o2) s 14
    ],
    testGroup "OwenCDF2"
    [
      testCase "OwenCDF2 - value 1" $ do
        x <- owenCDF2' 6 2 1 3 2
        (@~?) x 0.03257737810540227 9,
      testCase "OwenCDF2 - value 2" $ do
        x <- owenCDF2' 5 2 1 3 2
        (@~?) x 0.0353568969628651 8
    ],
    testGroup "OwenCDF1"
    [
      testCase "OwenCDF1 - value 1" $ do
        x <- owenCDF1' 6 2 1 3 2
        (@~?) x 0.1404299569049368 7,
      testCase "OwenCDF1 - value 2" $ do
        x <- owenCDF1' 5 2 1 3 2
        (@~?) x 0.1394458271284726 7
    ],
    testGroup "OwenCDF3"
    [
      testCase "OwenCDF3 - value 1" $ do
        x <- owenCDF3' 6 2 1 3 2
        (@~?) x 0.809137482066635 8,
      testCase "OwenCDF3 - value 2" $ do
        x <- owenCDF3' 5 2 1 3 2
        (@~?) x 0.806507459306199 8
    ],
    testGroup "OwenCDF"
    [
      testCase "The OwenCDF sum to one - test 1" $ do
        x1 <- owenCDF1' 6 2 1 2 1
        x2 <- owenCDF2' 6 2 1 2 1
        x3 <- owenCDF3' 6 2 1 2 1
        x4 <- owenCDF4' 6 2 1 2 1
        (@~?) (x1+x2+x3+x4) 1 15,
      testCase "The OwenCDF sum to one - test 2" $ do
        x1 <- owenCDF1' 5 2 1 2 1
        x2 <- owenCDF2' 5 2 1 2 1
        x3 <- owenCDF3' 5 2 1 2 1
        x4 <- owenCDF4' 5 2 1 2 1
        (@~?) (x1+x2+x3+x4) 1 15
    ]
  ]

approx :: RealFrac a => a -> Int -> a
approx x n = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

(@~?) :: (Show a, RealFrac a) => a -> a -> Int -> Assertion
(@~?) actual expected n = assertEqual "" (approx expected n) (approx actual n)

passValues :: [CDouble]
passValues = [1]

qt :: Double -> Double -> Double
qt nu = quantile (studentT nu)
