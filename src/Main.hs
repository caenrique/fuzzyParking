module Main (main) where
import FuzzyParking
import FuzzyLib
import Fuzzy
import Haskell
import Data.List

main :: IO ()
main = putStrLn $ (unlines . map (unwords . map (show))) vs
  where vs = [[a,x,fuzzyParking system [x,a]] | x <- [-50,-45..50], a <- [-180,-170..180]]
