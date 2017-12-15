module Main
  where
import Student

main :: IO()
main = do
  x <- studentCDF 0 0 [0]
  print x
