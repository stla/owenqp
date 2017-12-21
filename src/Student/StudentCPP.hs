{-# LANGUAGE ForeignFunctionInterface #-}
module Student.StudentCPP
  (studentCDFcpp)
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "studentCDF" c_studentCDF :: CDouble -> CSize ->
                         Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

studentCDFcpp :: (RealFloat a, Storable a, Integral b) =>
                 a -> b -> [a] -> IO (V.Vector a)
studentCDFcpp q nu delta = do
    case delta == [] of
      True -> return V.empty
      False -> do
        let deltavec = V.fromList (map realToFrac delta :: [CDouble])
        fptr <- mallocForeignPtrArray n
        V.unsafeWith deltavec $
          \v  -> withForeignPtr fptr $
            c_studentCDF (realToFrac q) (fromIntegral nu) v (fromIntegral n)
        return $ V.map realToFrac (V.unsafeFromForeignPtr0 fptr n)
  where n = length delta
