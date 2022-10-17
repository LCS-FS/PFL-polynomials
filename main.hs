import Data.List
type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

addNomial:: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

add :: Polynomial -> Polynomial -> Polynomial
add [a] b = addAux a b
add (a:as) b = addAux a (add as b) 


addAux:: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b:bs) = if snd a == snd b then addNomial a b : bs
                else b: addAux a bs


--normalize:: Polynomial -> Polynomial
--normalize p = add p []
-- usar add polinomio [] para somar monomoios do mesmo polinomio
