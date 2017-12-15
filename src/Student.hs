module Student
  where
import           Data.Number.Erf      (normcdf)
import qualified Data.Vector.Storable as V
import           Foreign.C.Types
import           Student.StudentCPP

pnorm :: CDouble -> CDouble
pnorm x = realToFrac $ normcdf (realToFrac x :: Double)

f :: CDouble -> CDouble -> CDouble
f q delta =
  if isInfinite delta
    then
      if signum q == signum delta
        then 0/0
        else if q>0 then 1 else 0
    else
      if q>0 then 1 else 0

-- TODO: check nu integer >=1, ou dans le code C ?
studentCDF :: CDouble -> CInt -> [CDouble] -> IO (V.Vector CDouble)
studentCDF q nu delta = do
  case nu >= (maxBound :: CInt) of
    True  -> return $ V.fromList (map (\x -> pnorm(q-x)) delta)
    False -> studentCDFcpp q nu delta
      -- case isInfinite q of
      --   True -> return $ V.fromList $ map (f q) delta
      --   False -> studentCDF q nu delta
