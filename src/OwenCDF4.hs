module OwenCDF4
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Internal.NormCDF
import           OwenCDF4.OwenCDF4CPP
import           Student.StudentCPP

owenCDF4 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenCDF4 nu t1 t2 delta1 delta2 = do
  case nu >= (maxBound :: CInt) of
    True -> return $ V.fromList $
              map (\(d1,d2) -> max 0 (pnorm(t2-d2)-pnorm(t1-d1)))
                  (zip delta1 delta2)
    False -> do
      let delta1delta2 = zip delta1 delta2
      let infinite1 = findIndices isInfinite delta1
      let infinite2 = findIndices
            (\(d1,d2) -> ((not . isInfinite) d1) && isInfinite d2) delta1delta2
      case infinite1 == [] && infinite2 == [] of
        True -> owenCDF4cpp nu t1 t2 delta1 delta2
        False -> do
          let pinfinite1 = findIndices (\x -> isInfinite x && x>0) delta1
          let ninfinite2 = findIndices (\x -> isInfinite x && x<0) delta2
          let finite = findIndices
                (\(d1,d2) -> ((not . isInfinite) d1) && ((not . isInfinite) d2))
                delta1delta2
          out0 <- owenCDF4cpp nu t1 t2 [delta1 !! i | i <- finite]
                                       [delta2 !! i | i <- finite]
          out1 <- studentCDFcpp t2 nu [delta2 !! i | i <- pinfinite1]
          out2 <- studentCDFcpp t1 nu [delta1 !! i | i <- ninfinite2]
          out <- VM.replicate n (0 :: CDouble)
          let step i j0 j1 j2
               | i == n = do
                    V.freeze out
               | otherwise = do
                    let delta1_i = delta1 !! i
                    case isInfinite delta1_i of
                      True -> do
                        case delta1_i > 0 of
                          True -> do
                            VM.write out i (out1 V.! j1)
                            step (i+1) j0 (j1+1) j2
                          False -> step (i+1) j0 j1 j2
                      False -> do
                        let delta2_i = delta2 !! i
                        case isInfinite delta2_i of
                          True -> do
                            case delta2_i < 0 of
                              True -> do
                                VM.write out i (out2 V.! j2)
                                step (i+1) j0 j1 (j2+1)
                              False -> step (i+1) j0 j1 j2
                          False -> do
                            VM.write out i (out0 V.! j0)
                            step (i+1) (j0+1) j1 j2
          step 0 0 0 0
        where n = length delta1
