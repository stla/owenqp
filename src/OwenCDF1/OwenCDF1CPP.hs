{-# LANGUAGE ForeignFunctionInterface #-}

module OwenCDF1.OwenCDF1CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenCDF1" c_OwenCDF1 :: CInt -> CDouble ->
                                       CDouble -> Ptr CDouble -> Ptr CDouble ->
                                        CSize -> Ptr CDouble -> IO (Ptr CDouble)
owenCDF1cpp :: CInt -> CDouble -> CDouble -> [CDouble] ->
                                              [CDouble] -> IO (V.Vector CDouble)
owenCDF1cpp nu t1 t2 delta1 delta2 = do
    case delta1 == [] of
      True -> return V.empty
      False -> do
        let delta1vec = V.fromList delta1
        let delta2vec = V.fromList delta2
        fptr <- mallocForeignPtrArray n
        V.unsafeWith delta1vec $
          \v1 -> V.unsafeWith delta2vec $
             \v2 -> withForeignPtr fptr $
                      c_OwenCDF1 nu t1 t2 v1 v2 (fromIntegral n)
        return $ V.unsafeFromForeignPtr0 fptr n
  where n = length delta1
