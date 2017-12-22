{-# LANGUAGE ForeignFunctionInterface #-}
module OwenT
  where
import           Foreign.C.Types

foreign import ccall unsafe "owent" owenT :: CDouble -> CDouble -> CDouble
