module Student
  (studentCDF)
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Internal.Infinite
import           Internal.NormCDF
import           Student.StudentCPP

_studentCDF :: CDouble -> CInt -> [CDouble] -> IO (V.Vector CDouble)
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
          out <- VM.replicate n (0 :: CDouble)
          let step i j
               | i == n = do
                    V.freeze out
               | otherwise = do
                    let delta_i = delta !! i
                    case isFinite delta_i of
                      True -> do
                        VM.write out i (out0 V.! j)
                        step (i+1) (j+1)
                      False -> do
                        case delta_i < 0 of
                          True -> do
                            VM.write out i (1 :: CDouble)
                            step (i+1) j
                          False -> step (i+1) j
          step 0 0
    where n = length delta

studentCDF :: CDouble -> CInt -> [CDouble] -> IO (V.Vector CDouble)
studentCDF q nu delta = do
  case delta == [] of
    True -> return V.empty
    False -> do
      case nu < 1 of
        True -> return $ V.replicate (length delta) (0/0)
        False -> do
          case nu == (maxBound :: CInt) of
            True  -> return $ V.fromList (map (\x -> pnorm(q-x)) delta)
            False -> do
              case isInfinite q of
                True -> return $ V.fromList $ map f delta
                False -> _studentCDF q nu delta
              where f d =
                      if isInfinite d
                        then
                          if signum q == signum d
                            then 0/0
                            else if q>0 then 1 else 0
                        else
                          if q>0 then 1 else 0
