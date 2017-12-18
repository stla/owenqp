module OwenCDF1.Internal
  (_owenCDF1)
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Internal.Infinite
import           OwenCDF1.OwenCDF1CPP
import           Student

__owenCDF1 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] ->
                                                           IO (V.Vector CDouble)
__owenCDF1 nu t1 t2 delta1 delta2 = do
  let delta1delta2 = zip delta1 delta2
  let infinite1 = findIndices isInfinite delta1
  let infinite2 = findIndices
        (\(d1,d2) -> isFinite d1 && isInfinite d2) delta1delta2
  case infinite1 == [] && infinite2 == [] of
    True -> owenCDF1cpp nu t1 t2 delta1 delta2
    False -> do
      let ninfinite2 = findIndices isMinusInfinite delta2
      let finite = findIndices
            (\(d1,d2) -> isFinite d1 && isFinite d2) delta1delta2
      out0 <- owenCDF1cpp nu t1 t2 [delta1 !! i | i <- finite]
                                   [delta2 !! i | i <- finite]
      out1 <- studentCDF t1 nu [delta1 !! i | i <- ninfinite2]
      out <- VM.replicate n (0 :: CDouble)
      let step i j0 j1
           | i == n = V.freeze out
           | otherwise = do
                case isInfinite (delta2 !! i) of
                  True -> do
                    VM.write out i (out1 V.! j1)
                    step (i+1) j0 (j1+1)
                  False -> do
                    case isInfinite (delta1 !! i) of
                      True -> step (i+1) j0 j1
                      False -> do
                        VM.write out i (out0 V.! j0)
                        step (i+1) (j0+1) j1
      step 0 0 0
    where n = length delta1

_owenCDF1 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] ->
                                                           IO (V.Vector CDouble)
_owenCDF1 nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> do
      case t2 >= t1 of
        True -> do
          case isMinusInfinite t2 of
            True -> case_t2_minusInfinite
            False -> studentCDF t1 nu delta1
        False -> do
          case isInfinite t1 of -- t1 = +oo
            True -> do
              let finite1 = findIndices isFinite delta1
              case finite1 == [] of
                True -> return $ V.replicate n (0/0)
                False -> do
                  case length finite1 == n of
                    True -> studentCDF t2 nu delta2
                    False -> do
                      out0 <- studentCDF t2 nu [delta2 !! i | i <- finite1]
                      out <- VM.replicate n (0/0 :: CDouble)
                      let step i j
                           | i == n = V.freeze out
                           | otherwise = do
                              case isInfinite (delta1 !! i) of
                                True -> step (i+1) j
                                False -> do
                                  VM.write out i (out0 V.! j)
                                  step (i+1) (j+1)
                      step 0 0
            False -> do
              case isInfinite t2 of -- t2 = -oo
                True -> case_t2_minusInfinite
                False -> do
                  let finite = findIndices
                                  (\(x,y) -> isFinite x && isFinite y)
                                  (zip delta1 delta2)
                  case length finite == n of
                    True -> owenCDF1cpp nu t1 t2 delta1 delta2
                    False -> do
                      case finite == [] of
                        True -> do
                          let ninfinite2 = findIndices isInfinite delta2
                          case ninfinite2 == [] of
                            True -> return $ V.replicate n 0
                            False -> do
                              case length ninfinite2 == n of
                                True -> studentCDF t1 nu delta1
                                False -> do
                                  out0 <- studentCDF t1 nu
                                                 [delta1 !! i | i <- ninfinite2]
                                  out <- VM.replicate n (0 :: CDouble)
                                  let step i j
                                        | i == n = V.freeze out
                                        | otherwise = do
                                            case isInfinite (delta2 !! i) of
                                              True -> do
                                                VM.write out i (out0 V.! j)
                                                step (i+1) (j+1)
                                              False -> step (i+1) j
                                  step 0 0
                        False -> do
                          let ninfinite2 = findIndices isInfinite delta2
                          out0 <- owenCDF1cpp nu t1 t2
                                              [delta1 !! i | i <- finite]
                                              [delta2 !! i | i <- finite]
                          out1 <- studentCDF t1 nu
                                             [delta1 !! i | i <- ninfinite2]
                          out <- VM.replicate n (0 :: CDouble)
                          let step i j0 j1
                                | i == n = V.freeze out
                                | otherwise = do
                                    case isFinite (delta1 !! i) && isFinite (delta2 !! i) of
                                      True -> do
                                        VM.write out i (out0 V.! j0)
                                        step (i+1) (j0+1) j1
                                      False -> do
                                        case isInfinite (delta2 !! i) of
                                          True -> do
                                            VM.write out i (out1 V.! j1)
                                            step (i+1) j0 (j1+1)
                                          False -> step (i+1) j0 j1
                          step 0 0 0
  where n = length delta1
        case_t2_minusInfinite :: IO (V.Vector CDouble)
        case_t2_minusInfinite = do
          let finite2 = findIndices isFinite delta2
          case finite2 == [] of
            True -> return $ V.replicate n (0/0)
            False -> do
              case length finite2 == n of
                True -> studentCDF t1 nu delta1
                False -> do
                  out0 <- studentCDF t1 nu [delta1 !! i | i <- finite2]
                  out <- VM.replicate n (0/0 :: CDouble)
                  let step i j
                       | i == n = V.freeze out
                       | otherwise = do
                          case isInfinite (delta2 !! i) of
                            True -> step (i+1) j
                            False -> do
                              VM.write out i (out0 V.! j)
                              step (i+1) (j+1)
                  step 0 0
