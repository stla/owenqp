module Main
  where
import Student

main :: IO()
main = do
  x <- studentCDF 0 5 [0, 1]
  print x
