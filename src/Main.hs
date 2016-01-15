module Main where
import FuzzyParking
import Data.List

main :: IO ()
main = putStrLn $ (unlines . map (unwords . map (show))) vs
  where vs = [[x,a,fuzzyParking x a] | x <- xs, a <- as]
        xs = [-50,-45..50]
        as = [-180,-165..180]
