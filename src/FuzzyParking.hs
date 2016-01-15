-- | El módulo 'FuzzyParking' define una función de inferencia concreta para el problema del
-- controlador de aparcamiento.
module FuzzyParking (fuzzyParking) where
import FuzzyLib
import Fuzzy
import Haskell

-- | Función que realiza la inferencia
fuzzyParking :: Inference -- [Float] -> Float
fuzzyParking sys (x:a:_) = (defuzmethod sys) $ zip3 centros base alphaList
  where alphaList = map (evalTerm (tnorma sys) (snorma sys) . tmap f . getTerm) $ reglas sys
        params = unzip . map (getParams . getConseq) $ (reglas sys)
        centros = fst params
        base = snd params
        f var = case var of
          "X"      -> x
          "Angulo" -> a
