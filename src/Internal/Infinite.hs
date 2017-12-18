module Internal.Infinite
  where

isPlusInfinite :: RealFloat a => a -> Bool
isPlusInfinite x = isInfinite x && x>0

isMinusInfinite :: RealFloat a => a -> Bool
isMinusInfinite x = isInfinite x && x<0

isFinite :: RealFloat a => a -> Bool
isFinite = not . isInfinite
