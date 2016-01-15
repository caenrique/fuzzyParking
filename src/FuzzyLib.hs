module FuzzyLib where
import Fuzzy

triangle :: (Float, Float, Float) -> MemFunc
triangle (a,b,c) f
  | f > a && f <= b = (f-a) / (b-a)
  | f > b && f < c  = 1 - (f-b) / (c-b)
  | otherwise       = 0

trapezoid :: (Float, Float, Float, Float) -> MemFunc   
trapezoid (a,b,c,d) f
  | f > a && f < b    = (f-a) / (b-a)
  | f > c && f < d    = 1 - (f-c) / (d-c)
  | f >= b && f <= c  = 1
  | otherwise         = 0

sramp :: (Float, Float) -> MemFunc
sramp (a,b) f
  | f > a && f < b = (f-a) / (b-a)
  | f >= b         = 1
  | otherwise      = 0

zramp :: (Float, Float) -> MemFunc
zramp (a,b) f
  | f > a && f < b = 1 - (f-a) / (b-a)
  | f <= a         = 1
  | otherwise      = 0

tnormMin :: Tnorm -- Fuzzy -> Fuzzy -> Fuzzy
tnormMin = min

tnormProd :: Tnorm -- Fuzzy -> Fuzzy -> Fuzzy
tnormProd = (*)

snormMax :: Snorm -- Fuzzy -> Fuzzy -> Fuzzy
snormMax = max

snormSum :: Snorm -- Fuzzy -> Fuzzy -> Fuzzy
snormSum a b = a + b - (a*b)

fuzzyMean :: Defuzmethod -- [(Float, Float, Float)] -> Float
fuzzyMean x = foldr (\(c,_,a) acc -> acc + (a*c)) 0 x /
  (foldr (\(_,_,a) acc -> acc + a) 0 x)

weightedFuzzyMean :: Defuzmethod -- [(Float, Float, Float)] -> Float
weightedFuzzyMean x = foldr (\(c,d,a) acc -> acc + (a*d*c)) 0 x /
  (foldr (\(_,d,a) acc -> acc + (a*d)) 0 x)
