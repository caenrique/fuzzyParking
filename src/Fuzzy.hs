module Fuzzy where

-- | Tipo de dato que representa un grado de verdad, es decir, un valor real entre 0 y 1.
type Fuzzy = Float

-- | Tipo de dato que describe una función de pertenencia, es decir,
-- una función que toma el valor de una variable y devuelve un grado de verdad.
type MemFunc = (Float -> Fuzzy)

-- | Tipo de dato que describe una variable del sistema difuso.
data Var = Var String | Val Float

-- | Tipo de dato que describe una proposición difusa.
data Term = (Term) :&& (Term) | (Term) :|| (Term) | (Var) := (MemFunc)

-- | Tipo de dato que describe el consecuente de una regla.
data Conseq = THEN Var (Float, Float)

-- | Tipo de dato que describe una T-norma.
type Tnorm = (Fuzzy -> Fuzzy -> Fuzzy)

-- | Tipo de dato que describe una S-norma.
type Snorm = (Fuzzy -> Fuzzy -> Fuzzy)

-- | Tipo de dato que describe una regla fuzzy.
data Rule = IF Term Conseq

-- | Tipo de dato que describe una base de reglas.
type RuleBase = [Rule]

-- | Tipo de dato que describe un método de defuzzificación.
type Defuzmethod = ([(Float, Float, Fuzzy)] -> Float) 

-- | Tipo de dato que describe un sistema difuso.
data FuzzySystem = FuzzySystem {
  reglas :: RuleBase,
  tnorma :: Tnorm,
  snorma :: Snorm,
  defuzmethod :: Defuzmethod
}

type Inference = (FuzzySystem -> [Float] -> Float)
