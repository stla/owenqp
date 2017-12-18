{-# LANGUAGE ForeignFunctionInterface #-}

module OwenQ2.OwenQ2CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenQ2" c_OwenQ2 :: CInt -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenQ2cpp :: CInt -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenQ2cpp nu t delta r = do
    case delta == [] of
      True -> return V.empty
      False -> do
        let deltavec = V.fromList delta
        let rvec = V.fromList r
        fptr <- mallocForeignPtrArray n
        V.unsafeWith deltavec $
          \v1 -> V.unsafeWith rvec $
             \v2 -> withForeignPtr fptr $ c_OwenQ2 nu t v1 v2 (fromIntegral n)
        return $ V.unsafeFromForeignPtr0 fptr n
  where n = length delta
