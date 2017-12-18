{-# LANGUAGE ForeignFunctionInterface #-}

module OwenCDF3.OwenCDF3CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenCDF3" c_OwenCDF3 :: CInt -> CDouble ->
                                       CDouble -> Ptr CDouble -> Ptr CDouble ->
                                        CSize -> Ptr CDouble -> IO (Ptr CDouble)
owenCDF3cpp :: CInt -> CDouble -> CDouble -> [CDouble] ->
                                              [CDouble] -> IO (V.Vector CDouble)
owenCDF3cpp nu t1 t2 delta1 delta2 = do
    case delta1 == [] of
      True -> return V.empty
      False -> do
        let delta1vec = V.fromList delta1
        let delta2vec = V.fromList delta2
        fptr <- mallocForeignPtrArray n
        V.unsafeWith delta1vec $
          \v1 -> V.unsafeWith delta2vec $
             \v2 -> withForeignPtr fptr $
                      c_OwenCDF3 nu t1 t2 v1 v2 (fromIntegral n)
        return $ V.unsafeFromForeignPtr0 fptr n
  where n = length delta1