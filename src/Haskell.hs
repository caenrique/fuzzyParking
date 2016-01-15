-- | El módulo 'Haskell' contiene varias funciones extras necesarias para su utilización en conjunto
-- con los módulos 'Fuzzy' y 'FuzzyLib'.
module Haskell where
import Fuzzy
import FuzzyLib

-- Esto son funciones extras que necesito para mi implementación --
-------------------------------------------------------------------

-- | La fución 'sera3' calcula el centro y la base de una regla de pertenencia
-- definida con 3 parámetros.
sera3 :: (Float, Float, Float) -> (Float, Float)
sera3 (a, b, c) = (b, c-a)

-- | La fución 'sera2' calcula el centro y la base de una regla de pertenencia
-- definida con 2 parámetros.
sera2 :: (Float, Float) -> (Float, Float)
sera2 (a, b) = (a + (b-a)/2, b-a)

-- | La fución 'evalTerm' evalúa un término difuso utilizando una T-norma y una S-norma,
-- devolviendo un valor Fuzzy, que representa el grado de activación.
evalTerm :: Tnorm -> Snorm -> Term -> Fuzzy
evalTerm and or (t1 :&& t2)          = (evalTerm and or t1) `and` (evalTerm and or t2)
evalTerm and or (t1 :|| t2)          = (evalTerm and or t1) `or`  (evalTerm and or t2)
evalTerm and or ((Val x) := memFunc) = memFunc x
evalTerm _   _   _                   = 0

-- | La fución 'tmap' aplica una fución de String a Float sobre la estructura de
-- un Term. Esto se utiliza por ejemplo para sustituir las variables por valores.
tmap :: (String -> Float) -> Term -> Term
tmap f (l :&& r) = (tmap f l) :&& (tmap f r)
tmap f (l :|| r) = (tmap f l) :|| (tmap f r)
tmap f ((Var x) := mem) = (Val $ f x) := mem

-- | La función 'getTerm' devuelve el término difuso de una regla.
getTerm :: Rule -> Term
getTerm (IF t _) = t 

-- | La función 'getConseq' devuelve el consecuente de una regla.
getConseq :: Rule -> Conseq
getConseq (IF _ c) = c

-- | La función 'getParams' devuelve los parámetros de un consecuente.
getParams :: Conseq -> (Float, Float)
getParams (THEN _ p) = p
