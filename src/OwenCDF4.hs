module OwenCDF4
  where
import qualified Data.Vector.Storable as V
import           Foreign.C.Types
import           OwenCDF4.OwenCDF4CPP

owenCDF4 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenCDF4 nu t1 t2 delta1 delta2 = do xx
