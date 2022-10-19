import Data.List
import Data.String
import Data.Char

type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

--Example:
--[(0, [('x', 2)]), (2, [('y', 1)]), (5, [('z', 1)]), (1, [('y', 1)]), (7, [('y', 2)])]
--[(7, [('y', 2)]), (3, [('y', 1)]), (5, [('z', 1)])]

-- ======================================================
--                    String Parsing
-- ===================================================
--ignorar espaços procurar "+"" ou "-" para separar 
--depois de encontrar um desses símbolos tem de aparecer o numero que é coeficiente e depois  

-- ======================================================
--                  Main Functions
-- ======================================================

add:: Polynomial -> Polynomial -> Polynomial
add a b = normalize (addBeforeNormalize a b)

normalize:: Polynomial -> Polynomial
normalize a = reverse (myisort (assort (addBeforeNormalize (map (nisort . nssort) ( remove0coeficients (map remove0exponents a))) [] )))

derivate:: Polynomial -> Char -> Polynomial
derivate a x = normalize (map (derivateNomial x) (normalize a))

multiply:: Polynomial -> Polynomial ->Polynomial
multiply a b = normalize (multiplyBefore a b)


-- need to sort alphabeticaly

-- ======================================================
--                  Aux Functions
-- ======================================================

-- =================== derivate ==========================
--percorro a lista ate encontrar a variavel e aplico derivate nela continuo a percorrer ate encontrar mais alguma se não encontrar então sigo
derivateNomial:: Char -> Nomial -> Nomial
derivateNomial _ (_, []) = (0,[]); --derivada de 2 em ordem a qualquer coisa é 0 (derivada sem variaveis)
derivateNomial x (coef,[(v,ex)]) = if v == x then (coef * ex,[(v,ex-1)]) else (0,[]) --derivada com 1 variavel
derivateNomial x (coef,l) = derivateNomialAux (coef,l) x --derivada com mais do que 1 variavel

derivateNomialAux:: Nomial -> Char -> Nomial
derivateNomialAux (coef,[(v,ex)]) x = if v == x then (coef * ex,[(v,ex-1)]) else (0,[]) 
derivateNomialAux (coef, (a:as) ) x = if (fst a) == x then (coef*(snd a), (fst a, (snd a)-1 ) : as) 
                                      else (fst (derivateNomialAux (coef, as) x), a : snd (derivateNomialAux (coef, as) x))
                                     


-- ======================= add ==========================

addBeforeNormalize :: Polynomial -> Polynomial -> Polynomial
addBeforeNormalize [a] b = addAux a b
addBeforeNormalize (a:as) b = addAux a (addBeforeNormalize as b)

addNomial:: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

addAux:: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b:bs) = if snd a == snd b then addNomial a b : bs
                else b: addAux a bs

-- ===================Multiply ==========================

-- [(1, [('x', 1)]), (2, [('y', 1)])] [(3, [('x', 1)]), (-4, [('y', 1)]), (5, [])]

multiplyBefore:: Polynomial -> Polynomial -> Polynomial
multiplyBefore [] b = []
multiplyBefore (a: as) b = addBeforeNormalize (multiplyAux a b) (multiplyBefore as b)

multiplyAux:: Nomial -> Polynomial -> Polynomial
multiplyAux a = map (nMulti a)

nMulti:: Nomial -> Nomial -> Nomial
nMulti a b = ((fst a)*(fst b), nMultiAux (snd a) (snd b));

nMultiAux :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
nMultiAux as b = foldr nMultiAux2 b as

nMultiAux2 :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
nMultiAux2 a [] = [a]
nMultiAux2 a (b: bs) = if(fst a == fst b) then (fst a, snd a + snd b) : bs
            else b: nMultiAux2 a bs
-- ==================== Normalize =======================

    -- polynomial alphabetical sort
aMin :: Polynomial -> Nomial
aMin [x] = x
aMin (x:xs) = if not (null (snd x)) && snd x < snd (aMin xs) then x else aMin xs

assort:: Polynomial -> Polynomial
assort [] = []
assort a = m : assort (Data.List.delete m a)
    where m = aMin a

    --polynomial exponent sort
myinsert :: Nomial -> Polynomial -> Polynomial
myinsert a [] = [a]
myinsert a (x:xs)
    | null (snd x) || null (snd a)              = x : myinsert a xs
    | snd (head (snd x)) > snd (head (snd a))   = a : x : xs
    | otherwise                                 = x : myinsert a xs

--myinsert a (x: xs) = if not (null (snd x)) && snd (head (snd x)) > snd (head (snd a)) then a : x : xs else
--    x : myinsert a xs

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

