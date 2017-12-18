{-# LANGUAGE ForeignFunctionInterface #-}

module OwenQ2.OwenQ2CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenQ2" c_OwenQ2 :: CSize -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenQ2cpp :: (RealFloat a, Storable a, Integral b) =>
             b -> a -> [a] -> [a] -> IO (V.Vector a)
owenQ2cpp nu t delta r = do
    case delta == [] of
      True -> return V.empty
      False -> do
        let deltavec = V.fromList (map realToFrac delta :: [CDouble])
        let rvec = V.fromList (map realToFrac r :: [CDouble])
        fptr <- mallocForeignPtrArray n
        V.unsafeWith deltavec $
          \v1 -> V.unsafeWith rvec $
             \v2 -> withForeignPtr fptr $
                c_OwenQ2 (fromIntegral nu) (realToFrac t) v1 v2 (fromIntegral n)
        return $ V.map realToFrac (V.unsafeFromForeignPtr0 fptr n)
  where n = length delta
