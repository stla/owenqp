module Internal.Infinite
  where
import Foreign.C.Types

isPlusInfinite :: CDouble -> Bool
isPlusInfinite x = isInfinite x && x>0

isMinusInfinite :: CDouble -> Bool
isMinusInfinite x = isInfinite x && x<0

isFinite :: CDouble -> Bool
isFinite = not . isInfinite
