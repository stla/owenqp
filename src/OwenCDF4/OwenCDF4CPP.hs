{-# LANGUAGE ForeignFunctionInterface #-}

module OwenCDF4.OwenCDF4CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenCDF4" c_OwenCDF4 :: CSize -> CDouble -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenCDF4cpp :: (RealFloat a, Storable a, Integral b) =>
               b -> a -> a -> [a] -> [a] -> IO (V.Vector a)
owenCDF4cpp nu t1 t2 delta1 delta2 = do
    case delta1 == [] of
      True -> return V.empty
      False -> do
        let delta1vec = V.fromList (map realToFrac delta1 :: [CDouble])
        let delta2vec = V.fromList (map realToFrac delta2 :: [CDouble])
        fptr <- mallocForeignPtrArray n
        V.unsafeWith delta1vec $
          \v1 -> V.unsafeWith delta2vec $
            \v2 -> withForeignPtr fptr $
                    c_OwenCDF4 (fromIntegral nu) (realToFrac t1) (realToFrac t2)
                               v1 v2 (fromIntegral n)
        return $ V.map realToFrac (V.unsafeFromForeignPtr0 fptr n)
  where n = length delta1
