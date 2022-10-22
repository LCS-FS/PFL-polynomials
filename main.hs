import Data.Char
import Data.List
import Data.String

-- Types created
type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

-- ====================================================
--                    String Functions
-- ====================================================

-- converts a polynomial into a string
stringify :: Polynomial -> String
stringify [] = "0"
stringify [x] = stringifyAux x
stringify (x : xs) =
  if fst (head xs) >= 0
    then stringifyAux x ++ " + " ++ stringify xs
    else stringifyAux x ++ " - " ++ stringify xs

-- converts a string into a polynomial
stringParsing:: String -> Polynomial
stringParsing str = map (parseNomial . (separate . replaceFirstLetter)) (separate (replace '-' "+-" (removeSpaces str)))

-- ======================================================
--                  Main Functions
-- ======================================================

-- adds two polynomials (represented by strings)
-- it parses the strings into polynomial types before adding them
-- normalizes the result and returns it as a string 
add :: String -> String -> String
add a b = stringify (normalizeBefore (addBeforeNormalize (stringParsing a) (stringParsing b)))

-- function to normalize any polynomial (represented by a string)
-- it parses the string a into polynomial type before normalizing the output into a string
normalize :: String -> String
normalize a = stringify (normalizeBefore (stringParsing a))

-- function to derivate any polynomial (represented by a string) in order of a variable (represented by a char)
-- calls auxiliary function derivateNomial for every monomial 
derivateInOrderTo :: String -> Char -> String
derivateInOrderTo a x = stringify (normalizeBefore (map (derivateNomial x) (normalizeBefore (stringParsing a))))

-- multiplies two polynomials (represented by strings)
-- it parses the strings into polynomial types before multiplying them
-- normalizes the result and returns it as a string 
multiply :: String -> String -> String
multiply a b = stringify (normalizeBefore (multiplyBefore (stringParsing a) (stringParsing b)))

-- ======================================================
--                  Aux Functions
-- ======================================================

-- ================= Normalize ==================

-- turns a polynomial into a normalized polynomial
normalizeBefore :: Polynomial -> Polynomial
normalizeBefore a = reverse (myisort (assort (addBeforeNormalize (map (nisort . nssort . addInner) (remove0coeficients (map remove0exponents a))) [])))

-- =================== Derivate ==================

-- derivates a nomial in order to a variable
-- if a nomial has multiple variables together it calls auxiliary function derivateNomialAux
derivateNomial :: Char -> Nomial -> Nomial
derivateNomial _ (_, []) = (0, []) -- derivative with no variables
derivateNomial x (coef, [(v, ex)]) = if v == x then (coef * ex, [(v, ex - 1)]) else (0, []) -- derivative with 1 variable
derivateNomial x (coef, l) = derivateNomialAux (coef, l) x -- derivative with multiple variables

-- derivates a nomial with multiple variables in order of a variable, recursively
derivateNomialAux :: Nomial -> Char -> Nomial
derivateNomialAux (coef, [(v, ex)]) x = if v == x then (coef * ex, [(v, ex - 1)]) else (0, []) --base case
derivateNomialAux (coef, (a : as)) x =
  if (fst a) == x
    then (coef * (snd a), (fst a, (snd a) - 1) : as)  --found variable to derivate
    else (fst (derivateNomialAux (coef, as) x), a : snd (derivateNomialAux (coef, as) x)) --hasn't found variable to derivate yet, continues recursively

-- ==================== add =====================

-- adds every nomial of the first polynomial to the second polinomial, recursively, with the help of an auxiliary funtion addAux
addBeforeNormalize :: Polynomial -> Polynomial -> Polynomial
addBeforeNormalize [] [] = []
addBeforeNormalize [] b = b
addBeforeNormalize [a] b = addAux a b
addBeforeNormalize (a : as) b = addAux a (addBeforeNormalize as b)

-- adds nomial to another nomial
addNomial :: Nomial -> Nomial -> Nomial
addNomial a b = (fst a + fst b, snd a)

-- adds a monomial to a polynomial
-- searches for the same variables in the polynomial and adds their coeficient
-- appends monomial to the polynomial otherwise
addAux :: Nomial -> Polynomial -> Polynomial
addAux a [] = [a]
addAux a (b : bs) =
  if snd a == snd b
    then addNomial a b : bs
    else b : addAux a bs

-- ===================Multiply ==========================

-- multiplies every monomial of the first polynomial with the second polynomial, with the aid of the auxiliary function multiplyAux
multiplyBefore :: Polynomial -> Polynomial -> Polynomial
multiplyBefore as b = foldr (\a -> addBeforeNormalize (multiplyAux a b)) [] as

-- multiplies the monomial with all monomials of the polynomial, with the aid of the auxiliary function nMulti
multiplyAux :: Nomial -> Polynomial -> Polynomial
multiplyAux a = map (nMulti a)

-- multiplies the coeficients of two monomials and calls nMultiAux to multiply their variables
nMulti :: Nomial -> Nomial -> Nomial
nMulti a b = ((fst a) * (fst b), nMultiAux (snd a) (snd b))

-- multiples every variable of the second input with the variables of the first input
-- calls an auxiliary function nMultiAux2
nMultiAux :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
nMultiAux a b = foldr nMultiAux2 b a

-- searches the same variable of the first input in the second input and adds their exponents
-- appends the first input to the second input otherwise
nMultiAux2 :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
nMultiAux2 a [] = [a]
nMultiAux2 a (b : bs) =
  if fst a == fst b
    then (fst a, snd a + snd b) : bs
    else b : nMultiAux2 a bs

-- ==================== Normalize =======================

-- polynomial alphabetical sort --

-- returns lowest monomial from a polynomial, alphabeticaly
aMin :: Polynomial -> Nomial
aMin [x] = x
aMin (x : xs) = if not (null (snd x)) && snd x < snd (aMin xs) then x else aMin xs

-- sorts every monomial in the polynomial, alphabeticaly, with selection sort
assort :: Polynomial -> Polynomial
assort [] = []
assort a = m : assort (delete m a)
  where
    m = aMin a

-- polynomial exponent sort --

-- inserts a monomial in the polynomial in order of its exponents
myinsert :: Nomial -> Polynomial -> Polynomial
myinsert a [] = [a]
myinsert a (x : xs)
  | null (snd x) || null (snd a) = x : myinsert a xs
  | snd (head (snd x)) > snd (head (snd a)) = a : x : xs
  | otherwise = x : myinsert a xs

-- sorts the monomials in the polynomial by exponent, with insertion sort
myisort :: Polynomial -> Polynomial
myisort = foldr myinsert []

-- monomial sort --

-- sorts the variables in the nomial, with the use of an auxiliary function myssort
nssort :: Nomial -> Nomial
nssort (a, x) = (a, myssort x)

-- sorts the list of variables, alphabeticaly, with selection sort
myssort :: [(Char, Int)] -> [(Char, Int)]
myssort [] = []
myssort a = m : myssort (delete m a)
  where
    m = nMin a

--returns the lowest (variable, exponent) tuple, alphabeticaly by the variable
nMin :: [(Char, Int)] -> (Char, Int)
nMin [x] = x
nMin (x : xs) = if fst x <= fst (nMin xs) then x else nMin xs

-- inserts a (variable, exponent) tuple in the list of variables in order of its exponent
ninsert :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
ninsert a [] = [a]
ninsert a (x : xs) =
  if snd x > snd a
    then a : x : xs
    else x : ninsert a xs

-- sorts the nomial's variables in order of exponent, with insertion sort
nisort :: Nomial -> Nomial
nisort (a, b) = (a, reverse (foldr ninsert [] b))

-- removes nomials like 0x^1
remove0coeficients :: Polynomial -> Polynomial
remove0coeficients [] = []
remove0coeficients (x : xs) =
  if fst x == 0
    then remove0coeficients xs
    else x : remove0coeficients xs

-- change monomials like 2x^0 to 2
remove0exponents :: Nomial -> Nomial
remove0exponents (a, x) = (a, remove0exponentsAux x)

-- auxiliary funtion that looks for (variable , 0) tuples and removes them
remove0exponentsAux :: [(Char, Int)] -> [(Char, Int)]
remove0exponentsAux [] = []
remove0exponentsAux (x : xs) =
  if snd x == 0
    then remove0exponentsAux xs
    else x : remove0exponentsAux xs

-- adds (variable, exponent) tuples with the same variable in a monomial, with the aid of an auxiliary function
addInner :: Nomial -> Nomial
addInner (a, b) = (a, addInnerAux1 b)

-- tries to add every  (variable, exponent) tuple to the rest of the list 
addInnerAux1 :: [(Char, Int)] -> [(Char, Int)]
addInnerAux1 [] = []
addInnerAux1 [x] = [x]
addInnerAux1 (x : xs) = addInnerAux2 x (addInnerAux1 xs)

-- adds a (variable, exponent) tuple to the list
-- if it finds a tuple in the list with the same variable, it adds their exponents
-- otherwise keeps recursively searching until it reaches the end of the list, where it appends the tuple
addInnerAux2 :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
addInnerAux2 a [] = [a]
addInnerAux2 a (x : xs) =
  if fst a == fst x
    then (fst a, snd a + snd x) : xs
    else x : addInnerAux2 a xs

-- ====================== Stringify =======================

-- auxiliary funtion that handles the convertion of the coeficient of the monomial to string
-- calls an auxiliary function to handle the convertion of the list of (variable, exponent) tuples to string
stringifyAux :: Nomial -> String
stringifyAux (-1,b) =
  if not (null b)
    then tail (stringifyAuxList b)
    else ""
stringifyAux (1,b) =
  if not (null b)
    then tail (stringifyAuxList b)
    else ""
stringifyAux (a, b) =
  if not (null b)
    then show (abs a) ++ stringifyAuxList b
    else show (abs a)

-- aux function that receives the variables and expoents and turns it into a string
-- considering if it doesn't have an expoent, it is 1, or more than 1
stringifyAuxList :: [(Char, Int)] -> String
stringifyAuxList [x] | snd x > 1     = '*' : fst x : "^" ++ show (snd x)
                     | snd x == 1    = '*' : [fst x]  
                     | otherwise     = ""
stringifyAuxList (x:xs) | snd x > 1  = '*' : fst x : "^" ++ show (snd x) ++ stringifyAuxList xs
                        | snd x == 1 = '*' : fst x  : stringifyAuxList xs
                        | otherwise  = fst x : stringifyAuxList xs


-- ====================== String Parsing =======================

--removes useless spaces (" ") from the string
removeSpaces :: String -> String 
removeSpaces = filter (/= ' ') 

-- searches for the first variable and if it doesn't have an expoent adds and underscore to represent it
-- if it does have a variable adds it separated by spaces
replaceFirstLetter :: String -> String
replaceFirstLetter [] = "* _"
replaceFirstLetter (x : xs) | x == '-' && isLetter (head xs)    = "-1*" ++ replaceFirstLetter xs
                            | isLetter x                        = " " ++ [x] ++ xs
                            | otherwise                         = x : replaceFirstLetter xs

-- replaces the delimitor char with the replacement string in the string so we can separate into lists after it
replace:: Char -> String -> String -> String
replace _ _ [] = []
replace del rep (x:xs) = 
  if x /= del
    then x : replace del rep xs
    else rep ++ replace del rep xs

-- if last element of the string is not a digit (which means it is a variable) adds the expoent '1' to it 
addPotency :: String -> String
addPotency str = if isDigit (last str) then str
                    else str ++ "^1"

--splits the string into a list of strings on the "+" character
separate :: String -> [String]
separate str = words (replace '+' " " str)

-- auxiliary function to convert a string with a number into a Integer
readAux :: String -> Int
readAux str = read str :: Int

-- parses [coeficient, variables] list of strings into a monomial
-- calls auxiliary functions to handle parsing of the coeficient and the variables 
parseNomial :: [String] -> Nomial
parseNomial [b] = (1, parseVariables b)
parseNomial [a, b] = (parseCoeficient a, parseVariables b)

-- parses coeficient string to int
-- if the coeficient is a multiplication like "2*3" it also multiplies them
parseCoeficient :: String -> Int
parseCoeficient "" = 1
parseCoeficient str = product (map readAux (words (replace '*' " " (init str))))

-- parses the string that represents the variables into a list of (variable, exponent) tuples
-- first splits the string by the '*' char
-- then calls auxiliary functions to simplify parsing of exponents
-- calls auxiliary function variablesAux to turn [variable, exponent] list of strings to (variable, exponent tuple)
parseVariables :: String -> [(Char, Int)]
parseVariables "_" = []
parseVariables str = map ((variablesAux . words) . replace '^' " " . addPotency) (words (replace '*' " " str))

-- auxiliary function that turns [variable, exponent] list of strings to (variable, exponent tuple)
variablesAux :: [String] -> (Char, Int)
variablesAux list = (head (head list), readAux (last list))
