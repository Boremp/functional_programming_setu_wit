-- EXERCISE FUNCTIONS
-- 1 Write a function add1. type signature for add1 :: Int -> Int
add1 :: Int -> Int
add1 x = x + 1

-- 2 Write a function always0. type signature for always0 :: Int-> Int
always0 :: Int-> Int
always0 _ = 0

-- 3 Write a function subtract’. type signature for subtract’ :: Int-> Int-> Int
subtract' :: Int-> Int-> Int
subtract' x y = x - y

-- 4 Write a function addmult. type signature for addmult :: Int-> Int -> Int-> Int
addmult :: Int-> Int -> Int-> Int
addmult p q r = (p+q)*r

-- 5 Write a function greaterThan0. That returns the String ”Yes!” if the number is 
-- greater than 0, and ”No!” otherwise.
greaterThan0 :: Int -> String
greaterThan0 x = if x > 0 then "Yes!" else "No!"

-- 6 Write a function pushOut. that takes a number and returns the number that is 
-- one step further from 0. That is,
-- • pushOut 3 is 4,
-- • pushOut (-10) is (-11), and
-- • pushOut 0 is 0.
-- That last one is because we don’t know which direction to go!.
pushOut :: Int -> Int
pushOut 0 = 0
pushOut x = if x > 0 then add1 x else x-1

pushOut' :: Int -> Int
pushOut' 0 = 0
pushOut' x  | x > 0 = add1 x 
            | True = x-1


-- 7 Write a function halve. that splits an even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve xs    |((length xs)`rem`2 == 0) = (take n xs, drop n xs) 
            |True = error "No havle for odd-lengthed list!"
                where n = ((length xs)`div`2)

-- 8 Write a function third. that returns the third element in a list that 
-- contains at least this many elements using:
-- 1. head and tail ;
-- 2. list indexing !! ;
-- 3. pattern matching.
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs!!2

third'' :: [a] -> a
third''(_:(_:(x:_))) = x

-- 9 Consider a function safetail that behaves in the same way as tail , except
-- that safetail maps the empty list to the empty list, whereas tail gives an error
-- in this case. Define safetail using:
-- 1. a conditional expression;
-- 2. guarded equations;
-- 3. pattern matching.
safetail:: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail':: [a] -> [a]
safetail' xs    |null xs = []
                |True = tail xs

safetail'':: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- 10 In a similar way to && in this section’s slides, show how the disjunction
-- operator || can be defined in three different ways using pattern matching.
--(Call it myOr)
myOr:: Bool -> Bool -> Bool
True `myOr` True = True 
True `myOr` False = True 
False `myOr` True = True 
False `myOr` False = False 

myOr':: Bool -> Bool -> Bool
False `myOr'` False = False
_ `myOr'` _ = True

myOr'' :: Bool -> Bool -> Bool
False `myOr''` b = b
True `myOr''` _ = True

-- 11 Given the function with the following type : lucky :: Integral a => a-> String
-- Write the function definition that returns the following strings given the 
-- following inputs:
-- 1. When input is 7, the output is the String ”Lucky you.. Proceed directly
-- to buy a lottery ticket.”.
-- 2. When input is 13, the output is the String ”You, sadly are quite unlucky.
-- Do not, under any circumstances, invest money today.”
-- 3. For any other input, the output is the String ”Mmmm.... Can’t really say....”

lucky :: Integral a => a-> String
lucky 7 = "Lucky you.. Proceed directly to buy a lottery ticket."
lucky 13 = "You, sadly are quite unlucky. Do not, under any circumstances, invest money today."
lucky x = "Mmmm.... Can't really say...."

-- 12 Given the two (Prelude) functions fst and snd who return the first and
-- second element of a 2-tuple. Write similar functions - first, second and third 
-- who return the first, second and third element of a three tuple.
firstoftuple:: (a,b,c) -> a
firstoftuple (x,_,_) = x
secondoftuple:: (a,b,c) -> b
secondoftuple (_,y,_) = y
thirdoftuple:: (a,b,c) -> c
thirdoftuple (_,_,z) = z

-- 13 The Luhn Algorithm is used to check bank card numbers for simple errors
-- such as mistyping a digit and proceeds as follows:
-- • consider each digit as a separate number;
-- • moving left, double every other number from the second last;
-- • subtract 9 from each number that is now greater than 9;
-- • add all the resulting numbers together;
-- • if the total is divisible by 10, the card number is valid.
-- Note that the rightmost digit is the check digit. Define a function
-- that doubles the a digit and subtracts 9 if the number is greater than 9.
luhnDouble :: Int -> Int
luhnDouble x    | xi > 9 = xi-9
                | True  =  xi
                where
                     xi = 2*x

-- Using luhnDouble and the integer remainder function mod , define a function
-- that decides if a four digit number is valid.
luhn :: Int -> Int-> Int-> Int-> Bool
luhn p q r s = if (total `mod` 10) == 0 then True else False
                where
                    total = (luhnDouble p) + q + (luhnDouble r) + s

-- 14 Using the same definition of the luhn algorithm and remembering that the
-- rightmost digit is the check digit, write a function that, given the leftmost 
-- three digits as per the previous example, calculates the check digit.
luhnGetCheck:: Int -> Int -> Int -> Int
luhnGetCheck p q r  | luhn p q r 0 = 0
                    | True = 10 - (((luhnDouble p) + q + (luhnDouble r)) `mod` 10)

-- FINISHED

-- #################################################################
-- #################################################################
-- #################################################################

-- SORTING EXERCISE

import Data.List ( sort, sortBy )

names :: [(String, String)]
names = [("Angela", "Merkel"), ("Joe", "Biden"), ("Michael D", "Higgins"), ("Boris", "Johnson")]

compareLastNames :: Ord a => (a, a) -> (a, a) -> Ordering
compareLastNames name1 name2 
           | lastName1 > lastName2 = GT 
           | lastName1 < lastName2 = LT
           | otherwise = EQ
    where   lastName1 = snd name1
            lastName2 = snd name2


compareFirstNames :: Ord a => (a,a) -> (a,a) -> Ordering
compareFirstNames name1 name2   | firstName1 > firstName2 = GT
                                | firstName1 < firstName2 = LT
                                | otherwise = EQ
                                where 
                                    firstName1 = fst name1
                                    firstName2 = fst name2

compareDescLastNames :: Ord a => (a, a) -> (a, a) -> Ordering
compareDescLastNames name1 name2 
           | lastName1 < lastName2 = GT 
           | lastName1 > lastName2 = LT
           | otherwise = EQ
    where   lastName1 = snd name1
            lastName2 = snd name2


compareLastNames' :: Ord a => (a, a) -> (a, a) -> Ordering
compareLastNames' name1 name2 
           | lastName1 > lastName2 = GT 
           | lastName1 < lastName2 = LT
           | otherwise = compareFirstNames name1 name2
    where   lastName1 = snd name1
            lastName2 = snd name2

-- FINISHED

-- #################################################################
-- #################################################################
-- #################################################################

-- ADDRESS GENERATING FUNCTION EXERCISE

data  Location = Wit | Itc | Oth | Ucd | Tcd
type Name = (String, String)

office :: Location -> String
office Wit = "Waterford institute of Technology, Cork Road, Waterford, Ireland, X91 K0EK."
office Itc  = "Carlow Institute of Technology, Dublin Road, Carlow, Ireland, R93 V960."
office Oth = "OTH Regensburg, Seybothstraße 2, 93053 Regensburg, Germany."
office Ucd = "University College Dublin, Belfield, Dublin 4, Ireland, D04 V1W8"
office Tcd = "Trinity College Dublin, the University of Dublin College Green Dublin 2,Ireland, D02 PN40 "

witOffice :: Name -> String
witOffice name =
  if lastName < "L"
    then nametext ++ ",  Lower Floor, Main Building  " ++ office Wit
    else nametext ++ ",  Top Floor, Main Building  " ++ office Wit
  where
    nametext = fst name ++ "  " ++ snd name
    lastName = snd name

itcOffice :: Name -> String
itcOffice name =   nametext ++ ",  " ++ office Itc
  where
    nametext = fst name ++ "  " ++ snd name

othOffice :: Name -> String
othOffice name =   nametext ++ ",  " ++ office Oth
  where
    nametext = snd name ++ ", " ++ fst name

ucdOffice :: Name -> String
ucdOffice name =   nametext ++ " Esq,  " ++ office Ucd
  where
    nametext = fst name ++ "  " ++ snd name

tcdOffice :: Name -> String
tcdOffice name =   snd name ++ ",  " ++ office Tcd


-- getLocation :: String -> Name -> String -- the next version is better
-- getLocation location name  = case location  of 
--                 "wit" -> witOffice name
--                 "itc" -> itcOffice name
--                 "oth" -> othOffice name
--                 _     -> fst name ++ "  " ++ snd name ++ ": Address unknown"

getLocation :: String -> (Name -> String)
getLocation location   = case location  of 
                "wit" -> witOffice 
                "itc" -> itcOffice 
                "oth" -> othOffice
                "ucd" -> ucdOffice
                "tcd" -> tcdOffice
                _     -> (\name -> fst name ++ "  " ++ snd name ++ ": Address unknown" ) 

addressLetter :: Name -> String -> String
addressLetter name location = getLocation location name 

-- FINISHED
