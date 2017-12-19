{-# LANGUAGE ForeignFunctionInterface #-}

module OwenCDF2.OwenCDF2CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenCDF2" c_OwenCDF2 :: CInt -> CSize -> CDouble ->
                               CDouble -> Ptr CDouble -> Ptr CDouble -> CSize ->
                                                 Ptr CDouble -> IO (Ptr CDouble)
owenCDF2cpp :: (RealFloat a, Storable a, Integral b) =>
               Int -> b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
owenCDF2cpp algo nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> do
      let delta1vec = V.fromList (map realToFrac delta1 :: [CDouble])
      let delta2vec = V.fromList (map realToFrac delta2 :: [CDouble])
      fptr <- mallocForeignPtrArray n
      V.unsafeWith delta1vec $
        \v1 -> V.unsafeWith delta2vec $
          \v2 -> withForeignPtr fptr $
            c_OwenCDF2 (fromIntegral algo) (fromIntegral nu) (realToFrac t1)
                       (realToFrac t2) v1 v2 (fromIntegral n)
      return $ V.map realToFrac (V.unsafeFromForeignPtr0 fptr n)
  where n = length delta1
