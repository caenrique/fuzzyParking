module Haskell where
import Fuzzy
import FuzzyLib

-- Esto son funciones extras que necesito para mi implementación --
-------------------------------------------------------------------

sera3 :: (Float, Float, Float) -> (Float, Float)
sera3 (a, b, c) = (b, c-a)

sera2 :: (Float, Float) -> (Float, Float)
sera2 (a, b) = (a + (b-a)/2, b-a)

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
