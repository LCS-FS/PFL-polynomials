import Data.List
type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

--[(0, [('x', 2)]), (2, [('y', 1)]), (5, [('z', 1)]), (1, [('y', 1)]), (7, [('y', 2)])]
--[(7, [('y', 2)]), (3, [('y', 1)]), (5, [('z', 1)])]

-- ======================================================
--                  Main Functions
-- ======================================================

add :: Polynomial -> Polynomial -> Polynomial
add [a] b = addAux a b
add (a:as) b = addAux a (add as b) 

normalize:: Polynomial -> Polynomial
normalize a = reverse (myisort (assort (add (map nssort ( remove0coeficients (map remove0exponents a))) [])))

-- need to sort alphabeticaly

-- ======================================================
--                  Aux Functions
-- ======================================================

-- ======================= add ==========================

addNomial:: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

addAux:: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b:bs) = if snd a == snd b then addNomial a b : bs
                else b: addAux a bs

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
nssort (a, x) = (a, Data.List.reverse (myssort x))

myssort:: [(Char, Int)] -> [(Char, Int)]
myssort [] = []
myssort a = m : myssort (Data.List.delete m a)
    where m = nMin a

nMin :: [(Char, Int)] -> (Char, Int) 
nMin [x] = x
nMin (x:xs) = if snd x <= snd (nMin xs) then x else nMin xs

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

