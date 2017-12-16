module Main
  where
import Student
import OwenCDF4

main :: IO()
main = do
--  x <- studentCDF 0 5 [0, 1]
  x <- owenCDF4 2 2 1 [1/0, 2, 2] [0, -1/0, 0]
  print x
