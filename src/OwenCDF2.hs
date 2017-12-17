module OwenCDF2
  (owenCDF2)
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Internal.Infinite
import           Internal.NormCDF
import           OwenCDF2.OwenCDF2CPP
import           Student

_owenCDF2 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
_owenCDF2 nu t1 t2 delta1 delta2 = do
  let delta1delta2 = zip delta1 delta2
  let finite = findIndices
        (\(d1,d2) -> isFinite d1 && isFinite d2) delta1delta2
  case length finite == n of
    True -> owenCDF2cpp nu t1 t2 delta1 delta2
    False -> do
      out0 <- owenCDF2cpp nu t1 t2 [delta1 !! i | i <- finite]
                                   [delta2 !! i | i <- finite]
      out <- VM.replicate n (0 :: CDouble)
      let step i j
           | i == n = do
                V.freeze out
           | otherwise = do
                case isInfinite (delta1 !! i) || isInfinite (delta2 !! j)  of
                  True -> step (i+1) j
                  False -> do
                    VM.write out i (out0 V.! j)
                    step (i+1) (j+1)
      step 0 0
    where n = length delta1

owenCDF2 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenCDF2 nu t1 t2 delta1 delta2 = do
  case t2 >= t1 of
    True -> do
      return $ V.replicate n (0 :: CDouble)
    False -> do
      case isInfinite t1 of -- t1 = +oo
        True -> do
          let finite = findIndices (isFinite) delta1 -- delta1 = +oo
          case length finite == n of
            True -> do
              s <- studentCDF t2 nu delta2
              return $ V.map ((-) 1) s
            False -> do
              case finite == [] of
                True -> return $ V.replicate n (0/0)
                False -> do
                  out0 <- studentCDF t2 nu [delta2 !! i | i <- finite]
                  out <- VM.replicate n (0/0 :: CDouble)
                  let step i j
                       | i == n = do
                            V.freeze out
                       | otherwise = do
                            case isInfinite (delta1 !! i)  of
                              True -> step (i+1) j
                              False -> do
                                VM.write out i (1 - (out0 V.! j))
                                step (i+1) (j+1)
                  step 0 0
        False -> do
          case isInfinite t2 of -- t2 = -oo
            True -> do
              let finite = findIndices (isFinite) delta2 -- delta2 = -oo
              case length finite == n of
                True -> studentCDF t1 nu delta1
                False -> do
                  case finite == [] of
                    True -> return $ V.replicate n (0/0)
                    False -> do
                      out0 <- studentCDF t1 nu [delta1 !! i | i <- finite]
                      out <- VM.replicate n (0/0 :: CDouble)
                      let step i j
                           | i == n = do
                                V.freeze out
                           | otherwise = do
                                case isInfinite (delta2 !! i)  of
                                  True -> step (i+1) j
                                  False -> do
                                    VM.write out i (out0 V.! j)
                                    step (i+1) (j+1)
                      step 0 0
            False -> _owenCDF2 nu t1 t2 delta1 delta2
  where n = length delta1