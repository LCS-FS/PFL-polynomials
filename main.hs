import Data.Char
import Data.List
import Data.String

type Nomial = (Int, [(Char, Int)])

type Polynomial = [Nomial]

-- Example:
-- [(3, [('x', 2)]), (2, [('y', 1)]), (5, [('z', 1)]), (1, [('y', 1)]), (7, [('y', 2)])]
-- [(7, [('y', 2)]), (3, [('y', 1)]), (5, [('z', 1)])]

-- ====================================================
--                    String Functions
-- ====================================================

stringify :: Polynomial -> String
stringify [] = "0"
stringify [x] = stringifyAux x
stringify (x : xs) =
  if fst (head xs) >= 0
    then stringifyAux x ++ " + " ++ stringify xs
    else stringifyAux x ++ " - " ++ stringify xs

stringParsing:: String -> Polynomial
stringParsing str = map (parseNomial . (separate . replaceFirstLetter)) (separate (replaceMinus (removeSpaces str)))

-- ======================================================
--                  Main Functions
-- ======================================================

add :: String -> String -> String
add a b = stringify (normalizeBefore (addBeforeNormalize (stringParsing a) (stringParsing b)))

normalize :: String -> String
normalize a = stringify (normalizeBefore (stringParsing a))

derivateInOrderTo :: String -> Char -> String
derivateInOrderTo a x = stringify (normalizeBefore (map (derivateNomial x) (normalizeBefore (stringParsing a))))

multiply :: String -> String -> String
multiply a b = stringify (normalizeBefore (multiplyBefore (stringParsing a) (stringParsing b)))

-- ======================================================
--                  Aux Functions
-- ======================================================

normalizeBefore :: Polynomial -> Polynomial
normalizeBefore a = reverse (myisort (assort (addBeforeNormalize (map (nisort . nssort . addInner) (remove0coeficients (map remove0exponents a))) [])))

-- =================== Derivate ==========================

derivateNomial :: Char -> Nomial -> Nomial
derivateNomial _ (_, []) = (0, []) -- derivada de 2 em ordem a qualquer coisa é 0 (derivada sem variaveis)
derivateNomial x (coef, [(v, ex)]) = if v == x then (coef * ex, [(v, ex - 1)]) else (0, []) -- derivada com 1 variavel
derivateNomial x (coef, l) = derivateNomialAux (coef, l) x -- derivada com mais do que 1 variavel

derivateNomialAux :: Nomial -> Char -> Nomial
derivateNomialAux (coef, [(v, ex)]) x = if v == x then (coef * ex, [(v, ex - 1)]) else (0, [])
derivateNomialAux (coef, (a : as)) x =
  if (fst a) == x
    then (coef * (snd a), (fst a, (snd a) - 1) : as)
    else (fst (derivateNomialAux (coef, as) x), a : snd (derivateNomialAux (coef, as) x))

-- ======================= add ==========================

addBeforeNormalize :: Polynomial -> Polynomial -> Polynomial
addBeforeNormalize [] [] = []
addBeforeNormalize [] b = b
addBeforeNormalize [a] b = addAux a b
addBeforeNormalize (a : as) b = addAux a (addBeforeNormalize as b)

addNomial :: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

addAux :: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b : bs) =
  if snd a == snd b
    then addNomial a b : bs
    else b : addAux a bs

-- ===================Multiply ==========================

multiplyBefore :: Polynomial -> Polynomial -> Polynomial
multiplyBefore as b = foldr (\a -> addBeforeNormalize (multiplyAux a b)) [] as

multiplyAux :: Nomial -> Polynomial -> Polynomial
multiplyAux a = map (nMulti a)

nMulti :: Nomial -> Nomial -> Nomial
nMulti a b = ((fst a) * (fst b), nMultiAux (snd a) (snd b))

nMultiAux :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
nMultiAux as b = foldr nMultiAux2 b as

nMultiAux2 :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
nMultiAux2 a [] = [a]
nMultiAux2 a (b : bs) =
  if fst a == fst b
    then (fst a, snd a + snd b) : bs
    else b : nMultiAux2 a bs

-- ==================== Normalize =======================

-- polynomial alphabetical sort
aMin :: Polynomial -> Nomial
aMin [x] = x
aMin (x : xs) = if not (null (snd x)) && snd x < snd (aMin xs) then x else aMin xs

assort :: Polynomial -> Polynomial
assort [] = []
assort a = m : assort (Data.List.delete m a)
  where
    m = aMin a

-- polynomial exponent sort
myinsert :: Nomial -> Polynomial -> Polynomial
myinsert a [] = [a]
myinsert a (x : xs)
  | null (snd x) || null (snd a) = x : myinsert a xs
  | snd (head (snd x)) > snd (head (snd a)) = a : x : xs
  | otherwise = x : myinsert a xs

myisort :: Polynomial -> Polynomial
myisort = foldr myinsert []

-- monomial sort
nssort :: Nomial -> Nomial
nssort (a, x) = (a, myssort x)

myssort :: [(Char, Int)] -> [(Char, Int)]
myssort [] = []
myssort a = m : myssort (Data.List.delete m a)
  where
    m = nMin a

nMin :: [(Char, Int)] -> (Char, Int)
nMin [x] = x
nMin (x : xs) = if fst x <= fst (nMin xs) then x else nMin xs

ninsert :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
ninsert a [] = [a]
ninsert a (x : xs) =
  if snd x > snd a
    then a : x : xs
    else x : ninsert a xs

nisort :: Nomial -> Nomial
nisort (a, b) = (a, reverse (foldr ninsert [] b))

-- remove 0x^1
remove0coeficients :: Polynomial -> Polynomial
remove0coeficients [] = []
remove0coeficients (x : xs) =
  if fst x == 0
    then remove0coeficients xs
    else x : remove0coeficients xs

-- change 2x^0 to 2
remove0exponents :: Nomial -> Nomial
remove0exponents (a, x) = (a, remove0exponentsAux x)

remove0exponentsAux :: [(Char, Int)] -> [(Char, Int)]
remove0exponentsAux [] = []
remove0exponentsAux (x : xs) =
  if snd x == 0
    then remove0exponentsAux xs
    else x : remove0exponentsAux xs

addInner :: Nomial -> Nomial
addInner (a, b) = (a, addInnerAux1 b)

addInnerAux1 :: [(Char, Int)] -> [(Char, Int)]
addInnerAux1 [] = []
addInnerAux1 [x] = [x]
addInnerAux1 (x : xs) = addInnerAux2 x (addInnerAux1 xs)

addInnerAux2 :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
addInnerAux2 a [] = [a]
addInnerAux2 a (x : xs) =
  if fst a == fst x
    then (fst a, snd a + snd x) : xs
    else x : addInnerAux2 a xs

-- ====================== Stringify =======================

stringifyAux :: Nomial -> String
stringifyAux (1,b) =
  if not (null b)
    then tail (stringifyAuxList b)
    else ""
stringifyAux (a, b) =
  if not (null b)
    then show (abs a) ++ stringifyAuxList b
    else show (abs a)

stringifyAuxList :: [(Char, Int)] -> String
stringifyAuxList [x] | snd x > 1     = '*' : fst x : "^" ++ show (snd x)
                     | snd x == 1    = '*' : [fst x]  
                     | otherwise     = ""
stringifyAuxList (x:xs) | snd x > 1  = '*' : fst x : "^" ++ show (snd x) ++ stringifyAuxList xs
                        | snd x == 1 = '*' : fst x  : stringifyAuxList xs
                        | otherwise  = fst x : stringifyAuxList xs
--stringifyAuxList [x] =
--  if snd x /= 0
--    then fst x : "^" ++ show (snd x)
--    else [fst x]
--stringifyAuxList (x : xs) =
--  if snd x /= 0
--    then fst x : "^" ++ show (snd x) ++ "*" ++ stringifyAuxList xs
--    else fst x : "*" ++ stringifyAuxList xs

-- ====================== String Parsing =======================

removeSpaces :: String -> String -- remove se os espaços
removeSpaces str = filter (\x -> x /= ' ') str

replaceMinus :: String -> String
replaceMinus [] = []
replaceMinus (x : xs) =
  if x /= '-'
    then x : replaceMinus xs
    else "+-" ++ replaceMinus xs

replacePlus :: String -> String
replacePlus [] = []
replacePlus (x : xs) =
  if x /= '+'
    then x : replacePlus xs
    else " " ++ replacePlus xs

replaceAsterics :: String -> String
replaceAsterics [] = []
replaceAsterics (x : xs) =
  if x /= '*'
    then x : replaceAsterics xs
    else " " ++ replaceAsterics xs

replacePotency :: String -> String
replacePotency [] = []
replacePotency (x : xs) =
  if x /= '^'
    then x : replacePotency xs
    else " " ++ replacePotency xs

replaceFirstLetter :: String -> String
replaceFirstLetter [] = "* _"
replaceFirstLetter (x : xs) =
  if isLetter x
    then " " ++ [x] ++ xs
    else x : replaceFirstLetter xs

addPotency :: String -> String
addPotency str = if isDigit (last str) then str
                    else str ++ "^1"

separate :: String -> [String]
separate str = words (replacePlus str)

readAux :: String -> Int
readAux str = read str :: Int

parseNomial :: [String] -> Nomial
parseNomial [b] = (1, parseVariables b)
parseNomial [a, b] = (parseCoeficient a, parseVariables b)

parseCoeficient :: String -> Int
parseCoeficient "" = 1
parseCoeficient str = product (map readAux (words (replaceAsterics (init str))))

parseVariables :: String -> [(Char, Int)]
parseVariables "_" = []
parseVariables str = map ((variablesAux . words) . replacePotency . addPotency) (words (replaceAsterics str))

variablesAux :: [String] -> (Char, Int)
variablesAux list = (head (head list), readAux (last list))

-- "x^2*y^3" -> ["x^2", "y^3"] -> ["x 2","y 3"] -> [["x", "2"], ["y", "3"]] -> [("x", 2), ("y", 3)]