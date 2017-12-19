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
  case length finite == n of
    True -> owenCDF1cpp algo nu t1 t2 delta1 delta2
    False -> do
      let infinite2 = findIndices isInfinite delta2
      case length infinite2 == n of
        True -> studentCDF t1 nu delta1
        False -> do
          case all isInfinite delta1 of
            True -> return $ V.replicate n 0
            False -> do
              out0 <- owenCDF1cpp algo nu t1 t2 [delta1 !! i | i <- finite]
                                                [delta2 !! i | i <- finite]
              out1 <- studentCDF t1 nu [delta1 !! i | i <- infinite2]
              out <- VM.replicate n (0 :: a)
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

__owenCDF1 :: forall a b. (RealFloat a, Storable a, Integral b, Bounded b) =>
              Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
__owenCDF1 algo nu t1 t2 delta1 delta2 = do
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
                      out <- VM.replicate n (0/0 :: a)
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
                False -> ___owenCDF1 algo nu t1 t2 delta1 delta2 -- ce cas est déjà traité dans __owenCDF1 !!!!!!!!
                  -- let finite = findIndices
                  --                 (\(x,y) -> isFinite x && isFinite y)
                  --                 (zip delta1 delta2)
                  -- case length finite == n of
                  --   True -> owenCDF1cpp nu t1 t2 delta1 delta2
                  --   False -> do
                  --     case finite == [] of
                  --       True -> do
                  --         let ninfinite2 = findIndices isInfinite delta2
                  --         case ninfinite2 == [] of
                  --           True -> return $ V.replicate n 0
                  --           False -> do
                  --             case length ninfinite2 == n of
                  --               True -> studentCDF t1 nu delta1
                  --               False -> do
                  --                 out0 <- studentCDF t1 nu
                  --                                [delta1 !! i | i <- ninfinite2]
                  --                 out <- VM.replicate n (0 :: CDouble)
                  --                 let step i j
                  --                       | i == n = V.freeze out
                  --                       | otherwise = do
                  --                           case isInfinite (delta2 !! i) of
                  --                             True -> do
                  --                               VM.write out i (out0 V.! j)
                  --                               step (i+1) (j+1)
                  --                             False -> step (i+1) j
                  --                 step 0 0
                  --       False -> do
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
                  --                   case isFinite (delta1 !! i) && isFinite (delta2 !! i) of
                  --                     True -> do
                  --                       VM.write out i (out0 V.! j0)
                  --                       step (i+1) (j0+1) j1
                  --                     False -> do
                  --                       case isInfinite (delta2 !! i) of
                  --                         True -> do
                  --                           VM.write out i (out1 V.! j1)
                  --                           step (i+1) j0 (j1+1)
                  --                         False -> step (i+1) j0 j1
                  --         step 0 0 0
  where n = length delta1
        case_t2_minusInfinite :: IO (V.Vector a)
        case_t2_minusInfinite = do
          let finite2 = findIndices isFinite delta2
          case finite2 == [] of
            True -> return $ V.replicate n (0/0)
            False -> do
              case length finite2 == n of
                True -> studentCDF t1 nu delta1
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
