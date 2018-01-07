{-# LANGUAGE ScopedTypeVariables #-}
module OwenCDF1
  (owenCDF1, owenCDF1')
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           OwenCDF1.Internal
import           Student

_owenCDF1 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
             Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
_owenCDF1 algo nu t1 t2 delta1 delta2
  | null delta1
    = return V.empty
  | nu < 1
    = return $ V.replicate n (0/0)
  | otherwise
    = do
      let delta1delta2 = zip delta1 delta2
      let higher = findIndices (uncurry (>)) delta1delta2
      if length higher == n
        then __owenCDF1 algo nu t1 t2 delta1 delta2
        else do
          let equal = findIndices (uncurry (==)) delta1delta2
          if length equal == n
            then studentCDF (min t1 t2) nu delta1
            else do
              let lower = findIndices (uncurry (<)) delta1delta2
              if length lower == n
                then __owenCDF1 algo nu t2 t1 delta2 delta1
                else do
                  out0 <- __owenCDF1 algo nu t1 t2
                                          [delta1 !! i | i <- lower]
                                          [delta2 !! i | i <- lower]
                  out1 <- studentCDF (min t1 t2) nu
                                     [delta1 !! i | i <- equal]
                  out2 <- __owenCDF1 algo nu t2 t1
                                     [delta2 !! i | i <- higher]
                                     [delta1 !! i | i <- higher]
                  out <- VM.replicate n (0 :: a)
                  let step i j0 j1 j2
                       | i == n = V.freeze out
                       | otherwise =
                            if (delta1 !! i) < (delta2 !! i)
                              then do
                                  VM.write out i (out0 V.! j0)
                                  step (i+1) (j0+1) j1 j2
                              else
                                if (delta1 !! i) == (delta2 !! i)
                                  then do
                                    VM.write out i (out1 V.! j1)
                                    step (i+1) j0 (j1+1) j2
                                  else do
                                    VM.write out i (out2 V.! j2)
                                    step (i+1) j0 j1 (j2+1)
                  step 0 0 0 0
  where n = length delta1

owenCDF1 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
            b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
owenCDF1 = _owenCDF1 1

owenCDF1' :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
             b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
owenCDF1' = _owenCDF1 2
