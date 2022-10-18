import Data.List
import Data.String

type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

--Example:
--[(0, [('x', 2)]), (2, [('y', 1)]), (5, [('z', 1)]), (1, [('y', 1)]), (7, [('y', 2)])]
--[(7, [('y', 2)]), (3, [('y', 1)]), (5, [('z', 1)])]

--Example broken:
--derivated do exemplo normalizado


-- ======================================================
--                    String Parsing
-- ===================================================
-- TO DO --

-- ======================================================
--                  Main Functions
-- ======================================================

add:: Polynomial -> Polynomial -> Polynomial
--add a b = normalize (addBeforaNormalize a b)
add [a] b = addAux a b
add (a:as) b = addAux a (add as b)

normalize:: Polynomial -> Polynomial
--normalize a = reverse (myisort (assort (addBeforaNormalize (map (nisort . nssort) ( remove0coeficients (map remove0exponents a))) [] ))) --not working
normalize a = reverse (myisort (assort (add (map nssort ( remove0coeficients (map remove0exponents a))) [])))

derivate:: Polynomial -> Polynomial
derivate = map derivateNomial

-- need to sort alphabeticaly

-- ======================================================
--                  Aux Functions
-- ======================================================

-- ======================= add ==========================

addBeforaNormalize :: Polynomial -> Polynomial -> Polynomial
addBeforaNormalize a [] = a
addBeforaNormalize [] a = a
addBeforaNormalize [a] b = addAux a b
addBeforaNormalize (a:as) b = addAux a (addBeforaNormalize as b)

derivateNomial:: Nomial -> Nomial --derivar com apenas uma variavel (para fazer com varias variaveis tinhamos de perguntar em ordem a x,y,z...)  
derivateNomial (_, []) = (0,[]);
derivateNomial (coef,[(v,ex)]) = (coef * ex,[(v,ex-1)])
derivateNomial (_,_) = error "multiple variables"

addNomial:: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

addAux:: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b:bs) = if snd a == snd b then addNomial a b : bs
                  else b: addAux a bs

--multiply nomials
--multiplyNomial:: Nomial -> Nomial -> Int
--multiplyNomial a b = (fst a * fst b, if(fst(head(fst a)) == fst(head(fst b))) then (snd(head(snd a) + snd(head(snd b)))) else "tenho de acabar isto")
                                              --multiplicamos sempre o coeficiente e depois
                                              --temos de ver se char for igual -> somar ints e manter o char
                                              -- se char for diferente -> juntar chars (por ordem alfabetica)
                                              -- fazer isso para a lista toda (é a parte mais tricky eu acho)
                                              --snd(head(snd (7, [('y', 2)]))) -> 2


-- ==================== Normalize =======================

    -- polynomial alphabetical sort
aMin :: Polynomial -> Nomial
aMin [x] = x
aMin (x:xs) = if fst (head (snd x)) <= fst (head(snd (aMin xs))) then x else aMin xs

assort:: Polynomial -> Polynomial
assort [] = []
assort a = m : assort (Data.List.delete m a)
    where m = aMin a

    --polynomial exponent sort
myinsert :: Nomial -> Polynomial -> Polynomial
myinsert a [] = [a]
myinsert a (x: xs) = if snd (head (snd x)) > snd (head (snd a)) then a : x : xs else
    x : myinsert a xs

myisort :: Polynomial -> Polynomial
myisort = foldr myinsert []

    -- monomial sort
nssort :: Nomial -> Nomial
nssort (a, x) = (a, myssort x)

myssort:: [(Char, Int)] -> [(Char, Int)]
myssort [] = []
myssort a = m : myssort (Data.List.delete m a)
    where m = nMin a

nMin :: [(Char, Int)] -> (Char, Int)
nMin [x] = x
nMin (x:xs) = if fst x <= fst (nMin xs) then x else nMin xs

ninsert:: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
ninsert a [] = [a]
ninsert a (x:xs) = if snd x > snd a then a : x : xs else
    x: ninsert a xs

nisort:: Nomial -> Nomial
nisort (a, b) = (a, reverse (foldr ninsert [] b))

    -- remove 0x^1
remove0coeficients:: Polynomial -> Polynomial
remove0coeficients [] = []
remove0coeficients (x:xs) = if fst x == 0 then remove0coeficients xs
                            else x : remove0coeficients xs
    --change 2x^0 to 2
remove0exponents:: Nomial -> Nomial
remove0exponents (a, x) = (a, remove0exponentsAux x)

remove0exponentsAux:: [(Char, Int)] -> [(Char, Int)]
remove0exponentsAux [] = []
remove0exponentsAux (x:xs) = if snd x == 0 then remove0exponentsAux xs
                            else x: remove0exponentsAux xs

