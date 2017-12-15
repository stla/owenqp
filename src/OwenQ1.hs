module OwenQ1
  where
import qualified Data.Vector.Storable  as V
import           Foreign.C.Types
import           Math.Gamma.Incomplete (lowerGammaHypGeom)
import           OwenQ1.OwenQ1CPP

owenQ1 :: CInt -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenQ1 nu t delta r = do
