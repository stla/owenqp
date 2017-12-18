module Internal.NormCDF
  where
import           Data.Number.Erf      (normcdf)

pnorm :: RealFloat a => a -> a
pnorm x = realToFrac $ normcdf (realToFrac x :: Double)
