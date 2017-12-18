module OwenCDF1
  (owenCDF1)
  where
import           Data.List                    (findIndices)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           OwenCDF1.Internal
-- import           OwenCDF3.Internal
import           Student

owenCDF1 :: CInt -> CDouble -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenCDF1 nu t1 t2 delta1 delta2 = do
  case delta1 == [] of
    True -> return V.empty
    False -> _owenCDF1 nu t1 t2 delta1 delta2
