import Data.List
type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

-- ======================================================
--                  Main Functions
-- ======================================================


add :: Polynomial -> Polynomial -> Polynomial
add [a] b = addAux a b
add (a:as) b = addAux a (add as b) 

normalize:: Polynomial -> Polynomial
normalize a = add (pssort (sortAllNomials a)) []
-- usar add polinomio [] para somar monomoios do mesmo polinomio

-- ======================================================
--                  Aux Functions
-- ======================================================

addNomial:: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

addAux:: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b:bs) = if snd a == snd b then addNomial a b : bs
                else b: addAux a bs
pMin :: Polynomial -> Nomial
pMin [x] = x
pMin (x:xs) = if snd x <= snd (pMin xs) then x else pMin xs

pssort:: Polynomial -> Polynomial
pssort [] = []
pssort a = m : pssort (delete m a)
    where m = pMin a

nssort :: Nomial -> Nomial
nssort (a, x) = (a, reverse (myssort x))

myssort:: [(Char, Int)] -> [(Char, Int)]
myssort [] = []
myssort a = m : myssort (delete m a)
    where m = nMin a

nMin :: [(Char, Int)] -> (Char, Int) 
nMin [x] = x
nMin (x:xs) = if snd x <= snd (nMin xs) then x else nMin xs

sortAllNomials:: Polynomial -> Polynomial
sortAllNomials [x] = [x]
sortAllNomials (x: xs) = nssort x : sortAllNomials xs 
