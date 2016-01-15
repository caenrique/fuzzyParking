module Main (main) where
import FuzzyParking
import FuzzyLib
import Fuzzy
import Haskell
import Data.List

main :: IO ()
main = putStrLn $ (unlines . map (unwords . map (show))) vs
  where vs = [[a,x,fuzzyParking system [x,a]] | x <- [-50,-45..50], a <- [-180,-170..180]]

system = FuzzySystem {
  reglas = [
     IF (((Var "X") := lE) :&& ((Var "Angulo") := rB)) $ THEN (Var "volante") (sera3 ps),
     IF (((Var "X") := lE) :&& ((Var "Angulo") := rU)) $ THEN (Var "volante") (sera3 ns),
     IF (((Var "X") := lE) :&& ((Var "Angulo") := rV)) $ THEN (Var "volante") (sera3 nm),
     IF (((Var "X") := lE) :&& ((Var "Angulo") := vE)) $ THEN (Var "volante") (sera3 nm),
     IF (((Var "X") := lE) :&& ((Var "Angulo") := lV)) $ THEN (Var "volante") (sera2 nb),
     IF (((Var "X") := lE) :&& ((Var "Angulo") := lU)) $ THEN (Var "volante") (sera2 nb),
     IF (((Var "X") := lE) :&& ((Var "Angulo") := lB)) $ THEN (Var "volante") (sera2 nb),

     IF (((Var "X") := lC) :&& ((Var "Angulo") := rB)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := lC) :&& ((Var "Angulo") := rU)) $ THEN (Var "volante") (sera3 ps),
     IF (((Var "X") := lC) :&& ((Var "Angulo") := rV)) $ THEN (Var "volante") (sera3 ns),
     IF (((Var "X") := lC) :&& ((Var "Angulo") := vE)) $ THEN (Var "volante") (sera3 nm),
     IF (((Var "X") := lC) :&& ((Var "Angulo") := lV)) $ THEN (Var "volante") (sera3 nm),
     IF (((Var "X") := lC) :&& ((Var "Angulo") := lU)) $ THEN (Var "volante") (sera2 nb),
     IF (((Var "X") := lC) :&& ((Var "Angulo") := lB)) $ THEN (Var "volante") (sera2 nb),

     IF (((Var "X") := cE) :&& ((Var "Angulo") := rB)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := cE) :&& ((Var "Angulo") := rU)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := cE) :&& ((Var "Angulo") := rV)) $ THEN (Var "volante") (sera3 ps),
     IF (((Var "X") := cE) :&& ((Var "Angulo") := vE)) $ THEN (Var "volante") (sera3 ze),
     IF (((Var "X") := cE) :&& ((Var "Angulo") := lV)) $ THEN (Var "volante") (sera3 ns),
     IF (((Var "X") := cE) :&& ((Var "Angulo") := lU)) $ THEN (Var "volante") (sera3 nm),
     IF (((Var "X") := cE) :&& ((Var "Angulo") := lB)) $ THEN (Var "volante") (sera3 nm),

     IF (((Var "X") := rC) :&& ((Var "Angulo") := rB)) $ THEN (Var "volante") (sera2 pb),
     IF (((Var "X") := rC) :&& ((Var "Angulo") := rU)) $ THEN (Var "volante") (sera2 pb),
     IF (((Var "X") := rC) :&& ((Var "Angulo") := rV)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := rC) :&& ((Var "Angulo") := vE)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := rC) :&& ((Var "Angulo") := lV)) $ THEN (Var "volante") (sera3 ps),
     IF (((Var "X") := rC) :&& ((Var "Angulo") := lU)) $ THEN (Var "volante") (sera3 ns),
     IF (((Var "X") := rC) :&& ((Var "Angulo") := lB)) $ THEN (Var "volante") (sera3 nm),

     IF (((Var "X") := rI) :&& ((Var "Angulo") := rB)) $ THEN (Var "volante") (sera2 pb),
     IF (((Var "X") := rI) :&& ((Var "Angulo") := rU)) $ THEN (Var "volante") (sera2 pb),
     IF (((Var "X") := rI) :&& ((Var "Angulo") := rV)) $ THEN (Var "volante") (sera2 pb),
     IF (((Var "X") := rI) :&& ((Var "Angulo") := vE)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := rI) :&& ((Var "Angulo") := lV)) $ THEN (Var "volante") (sera3 pm),
     IF (((Var "X") := rI) :&& ((Var "Angulo") := lU)) $ THEN (Var "volante") (sera3 ps),
     IF (((Var "X") := rI) :&& ((Var "Angulo") := lB)) $ THEN (Var "volante") (sera3 ns)
           ],
  tnorma = tnormProd,
  snorma = snormSum,
  defuzmethod = fuzzyMean 
}

le = (-40, -15)
lE = zramp le
lc = (-20, -10, 0)
lC = triangle lc
ce = (-5, 0, 5)
cE = triangle ce
rc = (0, 10, 20)
rC = triangle rc
ri = (15, 40)
rI = sramp ri

rb = (-190, -135, -80)
rB = triangle rb
ru = (-100, -66, -32)
rU = triangle ru
rv = (-50, -25, 0)
rV = triangle rv
ve = (-18, 0, 18)
vE = triangle ve
lv = (0, 25, 50)
lV = triangle lv
lu = (32, 66, 100)
lU = triangle lu
lb = (80, 135, 190)
lB = triangle lb

nb = (-30, -16)
nB = zramp nb
nm = (-26, -16, -6)
nM = triangle nm
ns = (-12, -6, 0)
nS = triangle ns
ze = (-6, 0, 6)
zE = triangle ze
ps = (0, 6, 12)
pS = triangle ps
pm = (6, 15, 24)
pM = triangle pm
pb = (16, 30)
pB = sramp pb
