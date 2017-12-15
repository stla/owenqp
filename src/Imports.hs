{-# LANGUAGE ForeignFunctionInterface #-}

module Imports
  where
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "pnorm" pnorm :: CDouble -> IO CDouble
