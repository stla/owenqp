{-# LANGUAGE ScopedTypeVariables #-}
module OwenCDF2
  (owenCDF2, owenCDF2')
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           OwenCDF2.Internal
import           OwenCDF4.Internal
import           Student

_owenCDF2 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
             Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
_owenCDF2 algo nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> do
      case nu < 1 of
        True -> return $ V.replicate n (0/0)
        False -> do
          let delta1delta2 = zip delta1 delta2
          let higher = findIndices (\(x,y) -> x>y) delta1delta2
          case length higher == n of
            True -> __owenCDF2 algo nu t1 t2 delta1 delta2
            False -> do
              let equal = findIndices (\(x,y) -> x==y) delta1delta2
              case length equal == n of
                True -> f delta1
                False -> do
                  let lower = findIndices (\(x,y) -> x<y) delta1delta2
                  case length lower == n of
                    True -> __owenCDF4 algo nu t2 t1 delta2 delta1
                    False -> do
                      out0 <- __owenCDF2 algo nu t1 t2
                                         [delta1 !! i | i <- higher]
                                         [delta2 !! i | i <- higher]
                      out1 <- f [delta1 !! i | i <- equal]
                      out2 <- __owenCDF4 algo nu t2 t1
                                         [delta2 !! i | i <- lower]
                                         [delta1 !! i | i <- lower]
                      out <- VM.replicate n (0 :: a)
                      let step i j0 j1 j2
                           | i == n = V.freeze out
                           | otherwise = do
                                case (delta1 !! i) < (delta2 !! i) of
                                  True -> do
                                      VM.write out i (out0 V.! j0)
                                      step (i+1) (j0+1) j1 j2
                                  False -> do
                                    case (delta1 !! i) == (delta2 !! i) of
                                      True -> do
                                        VM.write out i (out1 V.! j1)
                                        step (i+1) j0 (j1+1) j2
                                      False -> do
                                        VM.write out i (out2 V.! j2)
                                        step (i+1) j0 j1 (j2+1)
                      step 0 0 0 0
      where n = length delta1
            f :: [a] -> IO (V.Vector a)
            f delta = do
              case t2 < t1 && (delta /= []) of
                True -> do
                  s1 <- studentCDF t1 nu delta
                  s2 <- studentCDF t2 nu delta
                  return $ V.zipWith (-) s1 s2
                False -> return $ V.replicate (length delta) 0

owenCDF2 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
            b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
owenCDF2 = _owenCDF2 1

owenCDF2' :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
             b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
owenCDF2' = _owenCDF2 2
