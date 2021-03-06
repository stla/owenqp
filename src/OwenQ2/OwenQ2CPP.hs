{-# LANGUAGE ForeignFunctionInterface #-}
module OwenQ2.OwenQ2CPP
  (owenQ2cpp)
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenQ2" c_OwenQ2 :: CInt -> CSize -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenQ2cpp :: (RealFloat a, Storable a, Integral b) =>
             Int -> b -> a -> [a] -> [a] -> IO (V.Vector a)
owenQ2cpp algo nu t delta r = do
  case delta == [] of
    True -> return V.empty
    False -> do
      let deltavec = V.fromList (map realToFrac delta :: [CDouble])
      let rvec = V.fromList (map realToFrac r :: [CDouble])
      fptr <- mallocForeignPtrArray n
      _ <- V.unsafeWith deltavec $
        \v1 -> V.unsafeWith rvec $
          \v2 -> withForeignPtr fptr $
            c_OwenQ2 (fromIntegral algo) (fromIntegral nu) (realToFrac t) v1 v2
                     (fromIntegral n)
      return $ V.map realToFrac (V.unsafeFromForeignPtr0 fptr n)
  where n = length delta
