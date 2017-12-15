module OwenQ1
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Math.Gamma.Incomplete        (lowerGammaHypGeom)
import           OwenQ1.OwenQ1CPP

gammaQhalf :: CInt -> CDouble -> CDouble
gammaQhalf nu r = lowerGammaHypGeom (a/2) (r/2)
  where a = realToFrac nu :: CDouble

owenQ1 :: CInt -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenQ1 nu t delta r = do
  case nu >= (maxBound :: CInt) of
    True  -> return $ V.fromList (replicate (length delta) 0)
    False -> do
      let finiteIndices = findIndices (not . isInfinite) delta
      case length finiteIndices == n of
        True -> owenQ1cpp nu t delta r
        False -> do
          case finiteIndices == [] of
            True -> return $ V.fromList
              (map (\(delta, r) -> if delta>0 then 0 else gammaQhalf nu r)
                   (zip delta r))
            False -> do
              owen <- owenQ1cpp nu t [delta !! i | i <- finiteIndices]
                                     [r !! i | i <- finiteIndices]
              out <- VM.replicate n (0 :: CDouble)
              let step i j
                   | i == n = do
                        V.freeze out
                   | otherwise = do
                        let delta_i = delta !! i
                        let test = (not . isInfinite) delta_i
                        case test of
                          True -> do
                            VM.write out i (owen V.! j)
                            step (i+1) (j+1)
                          False -> do
                            case (delta_i < 0) of
                              True -> do
                                VM.write out i (gammaQhalf nu (r !! i))
                                step (i+1) j
                              False -> step (i+1) j
              step 0 0
    where n = length delta
