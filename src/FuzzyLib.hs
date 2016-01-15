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

tnormMin :: Tnorm
tnormMin = min

tnormProd :: Tnorm
tnormProd = (*)

snormMax :: Snorm
snormMax = max

snormSum :: Snorm
snormSum a b = a + b - (a*b)

fuzzyMean :: Defuzmethod
fuzzyMean x = foldr (\(c,_,a) acc -> acc + (a*c)) 0 x /
  (foldr (\(_,_,a) acc -> acc + a) 0 x)

weightedFuzzyMean :: Defuzmethod
weightedFuzzyMean x = foldr (\(c,d,a) acc -> acc + (a*d*c)) 0 x /
  (foldr (\(_,d,a) acc -> acc + (a*d)) 0 x)


-- Esto son funciones extras que necesito para mi implementación --
-------------------------------------------------------------------

sera3 :: (Float, Float, Float) -> (Float, Float)
sera3 (a, b, c) = (b, c-a)

sera2 :: (Float, Float) -> (Float, Float)
sera2 (a, b) = (b, b-a)

evalTerm :: Tnorm -> Snorm -> Term -> Fuzzy
evalTerm and or (t1 :&& t2)          = (evalTerm and or t1) `and` (evalTerm and or t2)
evalTerm and or (t1 :|| t2)          = (evalTerm and or t1) `or`  (evalTerm and or t2)
evalTerm and or ((Val x) := memFunc) = memFunc x
evalTerm _   _   _                   = 0

tmap :: (String -> Float) -> Term -> Term
tmap f (l :&& r) = (tmap f l) :&& (tmap f r)
tmap f (l :|| r) = (tmap f l) :|| (tmap f r)
tmap f ((Var x) := mem) = (Val $ f x) := mem

getTerm :: Rule -> Term
getTerm (IF t _) = t 

getConseq :: Rule -> Conseq
getConseq (IF _ c) = c

getParams :: Conseq -> (Float, Float)
getParams (THEN _ p) = p
