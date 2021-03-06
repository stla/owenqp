{-# LANGUAGE ScopedTypeVariables #-}
module OwenCDF4.Internal
  (__owenCDF4)
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Internal.Infinite
import           Internal.NormCDF
import           OwenCDF4.OwenCDF4CPP
import           Student

___owenCDF4 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
               Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
___owenCDF4 algo nu t1 t2 delta1 delta2 = do
  let delta1delta2 = zip delta1 delta2
  let infinite1 = findIndices isInfinite delta1
  let infinite2 = findIndices
        (\(d1,d2) -> isFinite d1 && isInfinite d2) delta1delta2
  case infinite1 == [] && infinite2 == [] of
    True -> owenCDF4cpp algo nu t1 t2 delta1 delta2
    False -> do
      let pinfinite1 = findIndices isPlusInfinite delta1
      let ninfinite2 = findIndices isMinusInfinite delta2
      let finite = findIndices
            (\(d1,d2) -> isFinite d1 && isFinite d2)
            delta1delta2
      out0 <- owenCDF4cpp algo nu t1 t2 [delta1 !! i | i <- finite]
                                        [delta2 !! i | i <- finite]
      out1 <- studentCDF t2 nu [delta2 !! i | i <- pinfinite1]
      out2 <- studentCDF t1 nu
                [delta1 !! i | i <- ninfinite2, i `notElem` pinfinite1]
      out <- VM.replicate n (0 :: a)
      let step i j0 j1 j2
           | i == n = V.freeze out
           | otherwise = do
                let delta1_i = delta1 !! i
                case isInfinite delta1_i of
                  True -> do
                    -- case delta1_i > 0 of -- inutile delta1 > delta2
                    --   True -> do
                    VM.write out i (out1 V.! j1)
                    step (i+1) j0 (j1+1) j2
                      -- False -> step (i+1) j0 j1 j2
                  False -> do
                    let delta2_i = delta2 !! i
                    case isInfinite delta2_i of
                      True -> do
                        -- case delta2_i < 0 of -- inutile car delta2 < delta1
                        --   True -> do
                          VM.write out i (1 - (out2 V.! j2))
                          step (i+1) j0 j1 (j2+1)
                          -- False -> step (i+1) j0 j1 j2
                      False -> do
                        VM.write out i (out0 V.! j0)
                        step (i+1) (j0+1) j1 j2
      step 0 0 0 0
    where n = length delta1

__owenCDF4 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
              Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
__owenCDF4 algo nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> do
      case t2 >= t1 of
        True -> do
          s1 <- studentCDF t1 nu delta1
          s2 <- studentCDF t2 nu delta2
          return $ V.zipWith (-) s2 s1
        False -> do
          case isInfinite t1 of -- t1 = +oo
            True -> return $ V.fromList $
              map (\(d1,d2) -> if isInfinite d1
                                 then 0/0
                                 else if isInfinite t2 && isInfinite d2
                                        then 0/0
                                        else 0)
                  (zip delta1 delta2)
            False -> do
              case isInfinite t2 of -- t2 = -oo
                True -> return $ V.fromList $
                         map (\x -> if isInfinite x then 0/0 else 0) delta2
                False -> do
                  case nu == (maxBound :: b) of -- mettre dans owenCDF4 ?
                    True -> return $ V.fromList $
                             map (\(d1,d2) -> max 0 (pnorm(t2-d2)-pnorm(t1-d1)))
                                 (zip delta1 delta2)
                    False -> ___owenCDF4 algo nu t1 t2 delta1 delta2
