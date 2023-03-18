import Data.Char (toUpper)

-- LIST A EXERCISES
-- EXERCISES ON HIGHER ORDER FUNCTIONS

-- 1 - Show how the list comprehension
-- [f x | x <- xs, p x]
-- can be re-expressed using the higher-order functions map and filter 
-- R: map f $ filter p xs

-- *** 2 - Without looking at the definitions from the standard prelude, define the
-- following higher-order library functions on lists.

-- 2.1 -Decide if all elements of a list satisfy a predicate:
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\x xs-> if f x then xs else False) True
            
-- 2.2 - Decide if all elements of a list satisfy a predicate:
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x xs -> if f x then True else xs) False

-- 2.3 - Select elements from a list while they satisfy a predicate:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x xs -> if f x then x:xs else []) []

-- 2.4 - Remove elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f = foldr (\x xs -> if f x then xs else x:xs) []

-- **** 3 - Redefine the functions "map f" and "filter p" using foldr
{- R:  
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f v [] = v
    foldr f v (x:xs) = f x (foldr f v xs)
-}

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs ->  f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4 - Noting that String is the same as [Char]. Define a function capitalises, of
-- type String → String, which takes a list of characters as its argument and
-- returns the same list as its value except that each lower-case letter has been
-- replaced by its upper-case equivalent. Thus,
-- capitalises ”Bohemian Rhapsody” = ”BOHEMIAN RHAPSODY”.
capitalises :: String -> String
capitalises = map toUpper
-- ?? capitalises = foldr (\x xs ->  toUpper x : xs) []

-- *** 5 - Define a function squareall :: [Int] → [Int] which takes a list of integers and
-- produces a list of the squares of those integers. For example,
-- squareall[6, 1, (−3)] = [36, 1, 9].
squareall :: [Int] -> [Int]
squareall = map (^2)
-- ?? squareall = foldr (\x xs ->  (^2) x : xs) []

-- **** 6 - Define a function nestedreverse which takes a list of strings as its argument
-- and reverses each element of the list and then reverses the resulting list. Thus,
-- nestedreverse [ ”in”, ”the”, ”end”] = [ ”dne”, ”eht”, ”ni” ].
nestedreverse :: [String] -> [String]
nestedreverse = map reverse . reverse
-- ?? nestedreverse = foldr (\x xs ->  reverse x : xs) [] . reverse

-- **** 7 - Define a function atfront :: a → [[a]] → [[a]] which takes an object and a
-- list of lists and prepends the object at the front of every component list. For
-- example,
-- atfront 7 [[1, 2], [], [3]] = [[7, 1, 2], [7], [7, 3]].
atfront :: a -> [[a]] -> [[a]]
atfront x = map (x:)
-- ?? atfront y = foldr (\x xs -> (y:)x:xs) []

-- *** 8 - Define a function lengths which takes a list of strings as its argument and
-- returns the list of their lengths. For example,
-- lengths [ ”the”, ”end”, ”is”,”nigh”] = [3, 3, 2, 4].
lengths :: [String] -> [Int]
lengths = map length
-- ?? lengths = foldr (\x xs -> length x : xs) []

-- *** 9 - Using the higher-order function map define a function sumsq which takes
-- an integer n as its argument and returns the sum of the squares of the first n
-- integers.
sumsq :: Int -> Int
sumsq n = sum $ map (^2) [1..n]

-- **** 10 - The function filter can be defined in terms of concat and map:
-- filter p = concat.map box where box x = ...
-- Write down the definition of box x
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = concat . map box 
        where 
            box x   | p x = [x]
                    | otherwise = []

-- *** 11 - Define a function wvowel (without vowels) which removes every occurrence
-- of a vowel from a list of characters.
wvowel :: String -> String
wvowel = filter (`notElem` vowels)
        where 
            vowels = vowel ++ map toUpper vowel
            vowel = ['a','e','i','o','u']

-- **** 12 - Define a function wiv (without internal vowels) which takes a list of strings
-- as its argument and removes every occurrence of a vowel from each element.
wiv :: [String] -> [String]
wiv = foldr (\x xs -> wvowel x : xs) []

-- EXERCISES ON HIGHER ORDER FUNCTIONS : PENDING EX.10
-- END LIST A EXERCISES

-- ##############################################################################################
-- ##############################################################################################

-- LIST B EXERCISES
-- EXERCISES ON FUNCTION APPLICATION AND FUNCTION COMPOSITION

-- 1 - Using $ Write bigCubes that takes a list and returns a list of cubes that are > 500
bigCubes :: [Int] -> [Int]
bigCubes ns = filter (>500) $ map (^3) ns

-- 2 Using $ and . Write lottaBiggest that takes a list and replicates the largest element 4 times.
lottaBiggest :: [Int] -> [Int]
lottaBiggest = replicate 4 . maximum

-- 3 Using $ Write powers that takes a number and creates a list of that number squared,
-- cubed, and quadrupled.
-- e.g. powers 2 = [4,8,16]
powers :: Num a => a -> [a]
powers n = map ($ n) [(^2),(^3),(^4)]

-- 4 Using $ Assume people are dining. We have a list of tip percents 
-- (assume people tip at different rates):
-- e.g. pcts = [0.15, 0.2, 0.21]
-- We have a list of bills (what people owe, minus tip)
-- e.g. amts = [20.5, 30, 25]
-- Write calcBill that takes amts and pcts and calculates what each person will
-- pay, based on their amt and pct. Then apply a 4% tax rate.
-- calcBillamtspcts [20.5, 30, 25] [0.15, 0.2, 0.21] = [24.518,37.44,31.46]
calcBillamtspcts :: [Float] -> [Float] -> [Float]
calcBillamtspcts xs ys = map (*1.04) $ zipWith (*) xs $ map (+1) ys

-- EXERCISES ON FUNCTION APPLICATION AND FUNCTION COMPOSITION : FINISHED
-- END LIST B EXERCISES

-- ##############################################################################################
-- ##############################################################################################

-- LIST C EXERCISES
-- EXERCISES ON FOLDR

-- 1 - Using the higher-order function foldr, define a function sumsq which takes
-- an integer n as its argument and returns the sum of the squares of the first n
-- integers. That is to say, sumsqn = 1^2 + 2^2 + 3^2 + ... + n^2.
sumsq' :: Integral a => a -> a
sumsq' n = foldr op 0 [1..n]
        where
            op x y = x*x + y

-- 2 - Define lengthr, which returns the number of elements in a list, using f oldr
lengthr :: [a] -> Int
lengthr = foldr (\_ y -> 1 + y) 0

-- ****** 3 - Define minlist, which returns the smallest integer in a non-empty list of
-- integers, using foldr1 . (foldr1 is a Prelude function - look it up yourself or
-- continue and come back to this) 
minlist :: [Int] -> Int
minlist = foldr1 f 
        where
            f x y   | x < y = x
                    | otherwise = y

-- **** 4 - Define myreverse, which reverses a list, using f oldr.
myreverse :: [a] -> [a]
myreverse = foldr (\x xs -> xs ++ [x]) [] 

-- **** 5 - Using f oldr , define a function remove which takes two strings as its arguments
-- and removes every letter from the second list that occurs in the first list.
-- For example,
-- remove "first" "second" = "econd".
-- Hint: Use a helper function in your lambda
remove :: String -> String -> String
remove unwanted zs = foldr (\x xs -> if helper x then x:xs else xs) [] zs
            where
                helper x = x `notElem` unwanted

-- ***** 6 - The function remdups removes adjacent duplicates from a list. For example,
-- remdups [1, 2, 2, 3, 3, 3, 1, 1] = [1, 2, 3, 1]
-- Define remdups using foldr (and using a helper method).
remdups :: [Int] -> [Int]
remdups = foldr (\x xs -> if helper (x:xs) then xs else x:xs) []
        where
            helper (_:[]) = False
            helper (x:x1:_) = x==x1
            
-- EXERCISES ON FOLDR : 
-- END LIST C EXERCISES
