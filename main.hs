import Data.List
type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

--canonycal:: Polynomial -> Polynomial
--canonycal 

addNomial:: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

add :: Polynomial -> Polynomial -> Polynomial
add [a] b = addAux a b
add (a:as) b = addAux a (add as b) 


addAux:: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b:bs) = if snd a == snd b then addNomial a b : bs
                else b: addAux a bs


--multiplyNomial:: Nomial -> Nomial -> Nomial
--multiplyNomial a b = (fst a * fst b, c)
--                    where c = nub [()]

-- [(1, [('x', 1)]), (2, [('x', 2)])]
-- [(1, [('x', 2)]), (2, [('x', 3)])]
