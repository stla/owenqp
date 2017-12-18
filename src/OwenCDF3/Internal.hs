{-# LANGUAGE ScopedTypeVariables #-}
module OwenCDF3.Internal
  (_owenCDF3)
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Internal.Infinite
import           OwenCDF3.OwenCDF3CPP
import           Student

__owenCDF3 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
              b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
__owenCDF3 nu t1 t2 delta1 delta2 = do
  let delta1delta2 = zip delta1 delta2
  let finite = findIndices
                 (\(d1,d2) -> isFinite d1 && isFinite d2) delta1delta2
  case length finite == n of
    True -> owenCDF3cpp nu t1 t2 delta1 delta2
    False -> do
      let infinite1 = findIndices isInfinite delta1
      case length infinite1 == n of
        True -> do
          s <- studentCDF t2 nu delta2
          return $ V.map ((-) 1) s
        False -> do
          case all isInfinite delta2 of
            True -> return $ V.replicate n 0
            False -> do
              out0 <- owenCDF3cpp nu t1 t2 [delta1 !! i | i <- finite]
                                           [delta2 !! i | i <- finite]
              out1 <- studentCDF t2 nu [delta2 !! i | i <- infinite1]
              out <- VM.replicate n (0 :: a)
              let step i j0 j1
                   | i == n = V.freeze out
                   | otherwise = do
                        case isInfinite (delta1 !! i) of
                          True -> do
                            VM.write out i (1 - (out1 V.! j1))
                            step (i+1) j0 (j1+1)
                          False -> do
                            case isInfinite (delta2 !! i) of
                              True -> step (i+1) j0 j1
                              False -> do
                                VM.write out i (out0 V.! j0)
                                step (i+1) (j0+1) j1
              step 0 0 0
  where n = length delta1

_owenCDF3 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
             b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
_owenCDF3 nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> do
      case t2 >= t1 of
        True -> do
          s <- studentCDF t2 nu delta2
          return $ V.map ((-) 1) s
        False -> do
          case isInfinite t1 of -- t1 = +oo
            True -> return $ V.fromList $
                              map (\x -> if isInfinite x then 0/0 else 0) delta1
            False -> do
              case isInfinite t2 of -- t2 = -oo
                True -> do
                  let finite2 = findIndices isFinite delta2
                  case finite2 == [] of
                    True -> return $ V.replicate n (0/0)
                    False -> do
                      case length finite2 == n of
                        True -> do
                          s <- studentCDF t1 nu delta1
                          return $ V.map ((-) 1) s
                        False -> do
                          out0 <- studentCDF t1 nu [delta1 !! i | i <- finite2]
                          out <- VM.replicate n (0/0 :: a)
                          let step i j
                               | i == n = V.freeze out
                               | otherwise = do
                                  case isInfinite (delta2 !! i) of
                                    True -> step (i+1) j
                                    False -> do
                                      VM.write out i (out0 V.! j)
                                      step (i+1) (j+1)
                          step 0 0
                False -> __owenCDF3 nu t1 t2 delta1 delta2
  where n = length delta1
