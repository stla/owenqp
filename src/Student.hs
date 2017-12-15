module Student
  where
import Student.StudentCPP
import Foreign.C.Types
import qualified Data.Vector.Storable         as V

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
  case isInfinite q of
    True -> return $ V.fromList $ map (f q) delta
    False -> studentCDFcpp q nu delta
