{-# LANGUAGE ForeignFunctionInterface #-}
module OwenExport
  where
import qualified Data.Vector.Storable as V
import           Foreign
import           Foreign.C
import           OwenCDF4
import           OwenQ1
import           OwenT
import           Student


foreign export ccall studentCDFexport :: Ptr CDouble -> Ptr CInt ->
                                 Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
studentCDFexport :: Ptr CDouble -> Ptr CInt -> Ptr CDouble ->
                                                Ptr CInt -> Ptr CDouble -> IO ()
studentCDFexport q nu delta n result = do
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

foreign export ccall owenQ1export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble ->
                                 Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
owenQ1export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
                                                Ptr CInt -> Ptr CDouble -> IO ()
owenQ1export nu t delta r n result = do
  nu <- peek nu
  t <- peek t
  n <- peek n
  delta <- peekArray (fromIntegral n) delta
  r <- peekArray (fromIntegral n) r
  (>>=) (fmap V.toList $ owenQ1' nu t delta r) (pokeArray result)

foreign export ccall owenCDF4export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble ->
                  Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
owenCDF4export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
                                Ptr CDouble -> Ptr CInt -> Ptr CDouble -> IO ()
owenCDF4export nu t1 t2 delta1 delta2 n result = do
  nu <- peek nu
  t1 <- peek t1
  t2 <- peek t2
  n <- peek n
  delta1 <- peekArray (fromIntegral n) delta1
  delta2 <- peekArray (fromIntegral n) delta2
  (>>=) (fmap V.toList $ owenCDF4 nu t1 t2 delta1 delta2) (pokeArray result)
