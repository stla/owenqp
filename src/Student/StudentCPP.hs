{-# LANGUAGE ForeignFunctionInterface #-}

module Student.StudentCPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "studentCDF" c_studentCDF :: CDouble -> CInt ->
                         Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

studentCDFcpp :: CDouble -> CInt -> [CDouble] -> IO (V.Vector CDouble)
studentCDFcpp q nu delta = do
    case delta == [] of
      True -> return V.empty
      False -> do
        let deltavec = V.fromList delta
        fptr <- mallocForeignPtrArray n
        V.unsafeWith deltavec $
          \v -> withForeignPtr fptr $ c_studentCDF q nu v (fromIntegral n)
        return $ V.unsafeFromForeignPtr0 fptr n
  where n = length delta
