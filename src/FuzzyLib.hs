-- | El módulo 'FuzzyLib' define un conjunto de funciones de pertenencia, T-normas, S-normas y métodos
-- de defuzzificación que pueden ser utilizados para definir sistemas difusos.
module FuzzyLib where
import Fuzzy

-- | La función 'triangle' es una función lineal a tramos en forma de triángulo,
-- definida por medio de tres parámetros.
triangle :: (Float, Float, Float) -> MemFunc
triangle (a,b,c) f
  | f > a && f <= b = (f-a) / (b-a)
  | f > b && f < c  = 1 - (f-b) / (c-b)
  | otherwise       = 0

-- | La función 'trapezoid' es una función lineal a tramos en forma de trapezoide,
-- definida por medio de cuatro parámetros.
trapezoid :: (Float, Float, Float, Float) -> MemFunc   
trapezoid (a,b,c,d) f
  | f > a && f < b    = (f-a) / (b-a)
  | f > c && f < d    = 1 - (f-c) / (d-c)
  | f >= b && f <= c  = 1
  | otherwise         = 0

-- | La función 'sramp' es una función lineal a tramos en forma de rampa ascendente,
-- definida por medio de dos parámetros.
sramp :: (Float, Float) -> MemFunc
sramp (a,b) f
  | f > a && f < b = (f-a) / (b-a)
  | f >= b         = 1
  | otherwise      = 0

-- | La función 'zramp' es una función lineal a tramos en forma de rampa descendente,
-- definida por medio de dos parámetros.
zramp :: (Float, Float) -> MemFunc
zramp (a,b) f
  | f > a && f < b = 1 - (f-a) / (b-a)
  | f <= a         = 1
  | otherwise      = 0

-- | La función 'tnormMin' es una T-norma que calcula el mínimo entre
-- dos grados de activación
tnormMin :: Tnorm -- Fuzzy -> Fuzzy -> Fuzzy
tnormMin = min

-- | La función 'tnormProd' es una T-norma que calcula el producto entre
-- dos grados de activación
tnormProd :: Tnorm -- Fuzzy -> Fuzzy -> Fuzzy
tnormProd = (*)

-- | La función 'snormMax' es una S-norma que calcula el máximo entre
-- dos grados de activación
snormMax :: Snorm -- Fuzzy -> Fuzzy -> Fuzzy
snormMax = max

-- | La función 'snormSum' es una S-norma que calcula la suma algebraica
-- entre dos grados de activación
snormSum :: Snorm -- Fuzzy -> Fuzzy -> Fuzzy
snormSum a b = a + b - (a*b)

-- | La función 'fuzzyMean' devuelve un método de defuzzyficación calculado como
-- la media de los centros de los consecuentes de cada regla, ponderados
-- por el grado de activación de la regla.
fuzzyMean :: Defuzmethod -- [(Float, Float, Float)] -> Float
fuzzyMean x = foldr (\(c,_,a) acc -> acc + (a*c)) 0 x /
  (foldr (\(_,_,a) acc -> acc + a) 0 x)

-- | La función 'weightedFuzzyMean' devuelve un método de defuzzyficación calculado
-- como la media de los centros de los consecuentes de cada regla, ponderados por
-- el producto entre el grado de activación de la regla y la base o el área del consecuente.
weightedFuzzyMean :: Defuzmethod -- [(Float, Float, Float)] -> Float
weightedFuzzyMean x = foldr (\(c,d,a) acc -> acc + (a*d*c)) 0 x /
  (foldr (\(_,d,a) acc -> acc + (a*d)) 0 x)
