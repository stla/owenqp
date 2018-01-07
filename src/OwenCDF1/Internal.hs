{-# LANGUAGE ScopedTypeVariables #-}
module OwenCDF1.Internal
  (__owenCDF1)
  where
import           Data.List                    (findIndices)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Internal.Infinite
import           OwenCDF1.OwenCDF1CPP
import           Student

___owenCDF1 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
               Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
___owenCDF1 algo nu t1 t2 delta1 delta2 = do
  let delta1delta2 = zip delta1 delta2
  let finite = findIndices
        (\(d1,d2) -> isFinite d1 && isFinite d2) delta1delta2
  if length finite == n
    then owenCDF1cpp algo nu t1 t2 delta1 delta2
    else do
      let infinite2 = findIndices isInfinite delta2
      if length infinite2 == n
        then studentCDF t1 nu delta1
        else
          if all isInfinite delta1
            then return $ V.replicate n 0
            else do
              out0 <- owenCDF1cpp algo nu t1 t2 [delta1 !! i | i <- finite]
                                                [delta2 !! i | i <- finite]
              out1 <- studentCDF t1 nu [delta1 !! i | i <- infinite2]
              out <- VM.replicate n (0 :: a)
              let step i j0 j1
                   | i == n = V.freeze out
                   | otherwise =
                        if isInfinite (delta2 !! i)
                          then do
                            VM.write out i (out1 V.! j1)
                            step (i+1) j0 (j1+1)
                          else
                            if isInfinite (delta1 !! i)
                              then step (i+1) j0 j1
                              else do
                                VM.write out i (out0 V.! j0)
                                step (i+1) (j0+1) j1
              step 0 0 0
  where n = length delta1

__owenCDF1 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
              Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
__owenCDF1 algo nu t1 t2 delta1 delta2
  | null delta1
    = return V.empty
  | t2 >= t1
    = if isMinusInfinite t2
        then case_t2_minusInfinite
        else studentCDF t1 nu delta1
  | isInfinite t1  -- t1 = +oo
    = do
      let finite1 = findIndices isFinite delta1
      if null finite1
        then return $ V.replicate n (0/0)
        else
          if length finite1 == n
            then studentCDF t2 nu delta2
            else do
              out0 <- studentCDF t2 nu [delta2 !! i | i <- finite1]
              out <- VM.replicate n (0/0 :: a)
              let step i j
                   | i == n = V.freeze out
                   | otherwise =
                      if isInfinite (delta1 !! i)
                        then step (i+1) j
                        else do
                          VM.write out i (out0 V.! j)
                          step (i+1) (j+1)
              step 0 0
  | otherwise
    = if isInfinite t2  -- t2 = -oo
      then case_t2_minusInfinite
      else ___owenCDF1 algo nu t1 t2 delta1 delta2 -- ce cas est déjà traité dans __owenCDF1 !!!!!!!!
        -- let finite = findIndices
        --                 (\(x,y) -> isFinite x && isFinite y)
        --                 (zip delta1 delta2)
        -- if length finite == n
        --   then owenCDF1cpp nu t1 t2 delta1 delta2
        --   else do
        --     if finite == []
        --       then do
        --         let ninfinite2 = findIndices isInfinite delta2
        --         if ninfinite2 == []
        --           then return $ V.replicate n 0
        --           else do
        --             if length ninfinite2 == n
        --               then studentCDF t1 nu delta1
        --               else do
        --                 out0 <- studentCDF t1 nu
        --                                [delta1 !! i | i <- ninfinite2]
        --                 out <- VM.replicate n (0 :: CDouble)
        --                 let step i j
        --                       | i == n = V.freeze out
        --                       | otherwise = do
        --                           if isInfinite (delta2 !! i)
        --                             then do
        --                               VM.write out i (out0 V.! j)
        --                               step (i+1) (j+1)
        --                             else step (i+1) j
        --                 step 0 0
        --       else do
        --         let ninfinite2 = findIndices isInfinite delta2
        --         out0 <- owenCDF1cpp nu t1 t2
        --                             [delta1 !! i | i <- finite]
        --                             [delta2 !! i | i <- finite]
        --         out1 <- studentCDF t1 nu
        --                            [delta1 !! i | i <- ninfinite2]
        --         out <- VM.replicate n (0 :: CDouble)
        --         let step i j0 j1
        --               | i == n = V.freeze out
        --               | otherwise = do
        --                   if isFinite (delta1 !! i) && isFinite (delta2 !! i)
        --                     then do
        --                       VM.write out i (out0 V.! j0)
        --                       step (i+1) (j0+1) j1
        --                     else do
        --                       if isInfinite (delta2 !! i)
        --                         then do
        --                           VM.write out i (out1 V.! j1)
        --                           step (i+1) j0 (j1+1)
        --                         else step (i+1) j0 j1
        --         step 0 0 0
  where n = length delta1
        case_t2_minusInfinite :: IO (V.Vector a)
        case_t2_minusInfinite = do
          let finite2 = findIndices isFinite delta2
          if null finite2
            then return $ V.replicate n (0/0)
            else
              if length finite2 == n
                then studentCDF t1 nu delta1
                else do
                  out0 <- studentCDF t1 nu [delta1 !! i | i <- finite2]
                  out <- VM.replicate n (0/0 :: a)
                  let step i j
                       | i == n = V.freeze out
                       | otherwise = 
                          if isInfinite (delta2 !! i)
                            then step (i+1) j
                            else do
                              VM.write out i (out0 V.! j)
                              step (i+1) (j+1)
                  step 0 0
