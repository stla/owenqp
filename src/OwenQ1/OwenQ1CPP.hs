{-# LANGUAGE ForeignFunctionInterface #-}

module OwenQ1.OwenQ1CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenQ1" c_OwenQ1 :: CInt -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenQ1cpp :: CInt -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenQ1cpp nu t delta r = do
    let deltavec = V.fromList delta
    let rvec = V.fromList r
    fptr <- mallocForeignPtrArray n
    V.unsafeWith deltavec $
      \v1 -> V.unsafeWith rvec $
         \v2 -> withForeignPtr fptr $ c_OwenQ1 nu t v1 v2 (fromIntegral n)
    return $ V.unsafeFromForeignPtr0 fptr n
  where n = length delta
