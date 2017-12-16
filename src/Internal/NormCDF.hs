module Internal.NormCDF
  where
import           Data.Number.Erf      (normcdf)
import           Foreign.C.Types
    
pnorm :: CDouble -> CDouble
pnorm x = realToFrac $ normcdf (realToFrac x :: Double)
