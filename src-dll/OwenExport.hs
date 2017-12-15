{-# LANGUAGE ForeignFunctionInterface #-}
module OwenExport
  where
import qualified Data.Vector.Storable as V
import           Foreign
import           Foreign.C
import           OwenT
import           Student


foreign export ccall studentExport :: Ptr CDouble -> Ptr CInt -> Ptr CDouble ->
                                                Ptr CInt -> Ptr CDouble -> IO ()
studentExport :: Ptr CDouble -> Ptr CInt -> Ptr CDouble ->
                                                Ptr CInt -> Ptr CDouble -> IO ()
studentExport q nu delta n result = do
  nu <- peek nu
  q <- peek q
  n <- peek n
  delta <- peekArray (fromIntegral n) delta
  out <- studentCDF q nu delta
  pokeArray result $ V.toList out

foreign export ccall owenTexport :: Ptr CDouble -> Ptr CDouble ->
                                                            Ptr CDouble -> IO ()
owenTexport :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
owenTexport h a result = do
  h <- peek h
  a <- peek a
  poke result (owenT h a)
--
-- foreign export ccall owenQexport :: Ptr CInt -> Ptr CDouble -> Ptr CDouble ->
--                                  Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
-- owenQexport :: Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
--                                                 Ptr CInt -> Ptr CDouble -> IO ()
-- owenQexport nu t delta r n result = do
--   nu <- peek nu
--   t <- peek t
--   n <- peek n
--   delta <- peekArray (fromIntegral n) delta
--   r <- peekArray (fromIntegral n) r
--   (>>=) (owenQ1 nu t delta r) (pokeArray result)
--
-- foreign export ccall powen4export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble ->
--                   Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
-- powen4export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
--                                 Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
-- powen4export nu t1 t2 delta1 delta2 n result = do
--   nu <- peek nu
--   t1 <- peek t1
--   t2 <- peek t2
--   n <- peek n
--   delta1 <- peekArray (fromIntegral n) delta2
--   delta2 <- peekArray (fromIntegral n) delta2
--   (>>=) (powen4 nu t1 t2 delta1 delta2) (pokeArray result)
