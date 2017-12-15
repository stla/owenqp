{-# LANGUAGE ForeignFunctionInterface #-}

module OwenQ1.OwenQ1CPP
  where
import qualified Data.Vector.Storable         as V
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "OwenQ1" c_OwenQ1 :: CInt -> CDouble ->
          Ptr CDouble -> Ptr CDouble -> CSize -> Ptr CDouble -> IO (Ptr CDouble)

owenQ1cpp :: Cint -> CDouble -> [CDouble] -> [CDouble] -> IO (V.Vector CDouble)
owenQ1cpp nu t delta r = 
