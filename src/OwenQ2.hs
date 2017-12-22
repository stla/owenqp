{-# LANGUAGE ScopedTypeVariables #-}
module OwenQ2
  (owenQ2, owenQ2')
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Internal.Infinite
import           Internal.NormCDF
import           Math.Gamma.Incomplete        (lowerGammaHypGeom)
import           OwenQ2.OwenQ2CPP

gammaQhalf :: forall a b. (Integral b, RealFloat a) => b -> a -> a
gammaQhalf nu r = 1 - lowerGammaHypGeom ((realToFrac nu :: a)/2) (r*r/2)

__owenQ2 :: forall a b. (RealFloat a, Storable a, Integral b) =>
            Int -> b -> a -> [a] -> [a] -> IO (V.Vector a)
__owenQ2 algo nu t delta r = do
    let finiteIndices = findIndices isFinite delta
    case length finiteIndices == n of
      True -> owenQ2cpp algo nu t delta r
      False -> do
        case finiteIndices == [] of
          True -> return $ V.fromList
            (map (\(x, y) -> if x>0 then 0 else gammaQhalf nu y)
                 (zip delta r))
          False -> do
            owen <- owenQ2cpp algo nu t [delta !! i | i <- finiteIndices]
                                        [r !! i | i <- finiteIndices]
            out <- VM.replicate n (0 :: a)
            let step i j
                 | i == n = V.freeze out
                 | otherwise = do
                      let delta_i = delta !! i
                      case isFinite delta_i of
                        True -> do
                          VM.write out i (owen V.! j)
                          step (i+1) (j+1)
                        False -> do
                          case delta_i < 0 of
                            True -> do
                              VM.write out i (gammaQhalf nu (r !! i))
                              step (i+1) j
                            False -> step (i+1) j
            step 0 0
  where n = length delta

_owenQ2 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
           Int -> b -> a -> [a] -> [a] -> IO (V.Vector a)
_owenQ2 algo nu t delta r = do
  case delta == [] of
    True -> return V.empty
    False -> do
      case nu < 1 of
        True -> return $ V.replicate (length delta) (0/0)
        False -> do
          case nu == (maxBound :: b) of
            True  -> return $ V.fromList $ map (pnorm . ((-) t)) delta
            False -> do
              case isPlusInfinite t of
                True -> return $ V.fromList $
                          map (\(x,y) -> if isPlusInfinite x
                                            then (0/0 :: a)
                                            else gammaQhalf nu y)
                              (zip delta r)
                False -> do
                  case isMinusInfinite t of
                    True -> return $ V.fromList $
                              map (\d -> if isMinusInfinite d
                                            then (0/0 :: a)
                                            else (0 :: a))
                                  delta
                    False -> __owenQ2 algo nu t delta r

owenQ2 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
          b -> a -> [a] -> [a] -> IO (V.Vector a)
owenQ2 = _owenQ2 1

owenQ2' :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
           b -> a -> [a] -> [a] -> IO (V.Vector a)
owenQ2' = _owenQ2 2
