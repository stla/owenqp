{-# LANGUAGE ScopedTypeVariables #-}
module Student
  (studentCDF)
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Internal.Infinite
import           Internal.NormCDF
import           Student.StudentCPP

_studentCDF :: forall a b. (RealFloat a, Storable a, Integral b) =>
               a -> b -> [a] -> IO (V.Vector a)
_studentCDF q nu delta = do
  let finiteIndices = findIndices isFinite delta
  case length finiteIndices == n of
    True -> studentCDFcpp q nu delta
    False -> do
      case finiteIndices == [] of
        True -> return $ V.fromList $
                           map (\x -> if x>0 then 0 else 1) delta
        False -> do
          out0 <- studentCDFcpp q nu ([delta !! i | i <- finiteIndices])
          out <- VM.replicate n (0 :: a)
          let step i j
               | i == n = V.freeze out
               | otherwise = do
                    let delta_i = delta !! i
                    case isFinite delta_i of
                      True -> do
                        VM.write out i (out0 V.! j)
                        step (i+1) (j+1)
                      False -> do
                        case delta_i < 0 of
                          True -> do
                            VM.write out i 1
                            step (i+1) j
                          False -> step (i+1) j
          step 0 0
    where n = length delta

studentCDF :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
              a -> b -> [a] -> IO (V.Vector a)
studentCDF q nu delta = do
  case delta == [] of
    True -> return V.empty
    False -> do
      case nu < 1 of
        True -> return $ V.replicate (length delta) (0/0)
        False -> do
          case nu == (maxBound :: b) of
            True  -> return $ V.fromList (map (\x -> pnorm(q-x)) delta)
            False -> do
              case isInfinite q of
                True  -> return $ V.fromList $ map f delta
                False -> _studentCDF q nu delta
              where f d =
                      if isInfinite d
                        then
                          if signum q == signum d
                            then 0/0
                            else if q>0 then 1 else 0
                        else
                          if q>0 then 1 else 0
