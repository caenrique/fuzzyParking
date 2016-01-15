module Fuzzy where

type Fuzzy = Float
type MemFunc = (Float -> Fuzzy)
data Var = Var String | Val Float

data Term = (Term) :&& (Term) | (Term) :|| (Term) | (Var) := (MemFunc)
data Conseq = THEN Var (Float, Float)

type Tnorm = (Fuzzy -> Fuzzy -> Fuzzy)
type Snorm = (Fuzzy -> Fuzzy -> Fuzzy)

data Rule = IF Term Conseq
type RuleBase = [Rule]

type Defuzmethod = ([(Float, Float, Fuzzy)] -> Float) 

data FuzzySystem = FuzzySystem {
  reglas :: RuleBase,
  tnorma :: Tnorm,
  snorma :: Snorm,
  defuzmethod :: Defuzmethod
}
