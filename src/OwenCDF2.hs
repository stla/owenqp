module OwenCDF2
  (owenCDF2)
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           OwenCDF2.Internal
import           OwenCDF4.Internal
import           Student

owenCDF2 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] ->
                                                           IO (V.Vector CDouble)
owenCDF2 nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True  -> return V.empty
    False -> _owenCDF2 nu t1 t2 delta1 delta2
