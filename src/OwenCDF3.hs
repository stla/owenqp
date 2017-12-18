{-# LANGUAGE ScopedTypeVariables #-}
module OwenCDF3
  (owenCDF3)
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           OwenCDF3.Internal
import           Student

owenCDF3 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] ->
                                                           IO (V.Vector CDouble)
owenCDF3 nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> do
      case nu < 1 of
        True -> return $ V.replicate n (0/0)
        False -> do
          let delta1delta2 = zip delta1 delta2
          let higher = findIndices (\(x,y) -> x>y) delta1delta2
          case length higher == n of
            True -> _owenCDF3 nu t1 t2 delta1 delta2
            False -> do
              let equal = findIndices (\(x,y) -> x==y) delta1delta2
              case length equal == n of
                True -> do
                  s <- studentCDF (max t1 t2) nu delta1
                  return $ V.map ((-) 1) s
                False -> do
                  let lower = findIndices (\(x,y) -> x<y) delta1delta2
                  case length lower == n of
                    True -> _owenCDF3 nu t2 t1 delta2 delta1
                    False -> do
                      out0 <- _owenCDF3 nu t1 t2 [delta1 !! i | i <- lower]
                                                 [delta2 !! i | i <- lower]
                      out1 <- studentCDF (max t1 t2) nu
                                         [delta1 !! i | i <- equal]
                      out2 <- _owenCDF3 nu t2 t1 [delta2 !! i | i <- higher]
                                                 [delta1 !! i | i <- higher]
                      out <- VM.replicate n (0 :: CDouble)
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
                                        VM.write out i (1 - out1 V.! j1)
                                        step (i+1) j0 (j1+1) j2
                                      False -> do
                                        VM.write out i (out2 V.! j2)
                                        step (i+1) j0 j1 (j2+1)
                      step 0 0 0 0
      where n = length delta1
