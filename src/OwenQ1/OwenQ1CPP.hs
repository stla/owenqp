{-# LANGUAGE ForeignFunctionInterface #-}
module OwenQ1.OwenQ1CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenQ1" c_OwenQ1 :: CSize -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenQ1cpp :: (RealFloat a, Storable a, Integral b) =>
             b -> a -> [a] -> [a] -> IO (V.Vector a)
owenQ1cpp nu t delta r = do
    case delta == [] of
      True -> return V.empty
      False -> do
        let deltavec = V.fromList (map realToFrac delta :: [CDouble])
        let rvec = V.fromList (map realToFrac r :: [CDouble])
        fptr <- mallocForeignPtrArray n
        V.unsafeWith deltavec $
          \v1 -> V.unsafeWith rvec $
            \v2 -> withForeignPtr fptr $
                c_OwenQ1 (fromIntegral nu) (realToFrac t) v1 v2 (fromIntegral n)
        return $ V.map realToFrac (V.unsafeFromForeignPtr0 fptr n)
  where n = length delta
