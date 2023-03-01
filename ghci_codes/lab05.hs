-- EXERCISE LIST COMPREHENSIONS
import Data.Char (toUpper)
-- 1 - Using list comprehension define the following list
l1 = [1,2,3,4,5,6]
a1 = [x | x <- [1..6]]

-- 2 - Using list comprehension define the following list
l2 = [10,20,30,40,50,60]
a2 = [x*10 |x <- [1..6]]

-- 3 - Using list comprehension define the following list
l3 = [(1,1),(2,2),(3,3),(4,4)]
a3 = [(x,x) |x <- [1..4]]

-- 4 - Using list comprehension define the following list
l4 = [(1,2),(2,3),(3,4),(4,5)]
a4 = [(x,x+1) |x <- [1..4]]

-- 5 - Using list comprehension define the following list 
-- (note that the second element in the 2-tuple is always 1)
l5 = [(1,1),(2,1),(3,1),(4,1),(5,1)]
a5 = [(x,1) |x <- [1..5]]

-- Using list comprehension define the list of squares of 
-- the values between (and including) 1 and 10.
l6 = [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
a6 = [(x,x^2) |x <- [1..10]]

-- 7 - Write down the values as defined in the following lists f1, f2, f3. 
l71 :: [(Int, Int)]
l71 = [(x, y) | x <-[1..3], y<- [4..5]]
a71 = [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

l72 :: [(Int, Int)]
l72 = [(x, y) | y<- [4..5], x <-[1..3]]
a72 = [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

l73 :: [(Int, Int)]
l73 = [(y, x) | x <-[1..3], y<- [4..5]]
a73 = [(4,1),(5,1),(4,2),(5,2),(4,3),(5,3)]

-- 8 - Given the following definition of 
isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)
-- Write down the values as defined in the following list.
l8 = [2*n | n <- [2,4,7], isEven n, n>3]
a8 = [8]

check1to8 =all (==True) [l1==a1,l2==a2,l3==a3,l4==a4,l5==a5,l6==a6,l71==a71,l72==a72,l73==a73,l8==a8]

-- 9 - Give a definition of a function which doubles all 
-- the elements of a list of integers.
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [x*2 | x <- xs]

-- 10 - Give a definition of a function
-- which converts all small letters in a String into capitals.
-- Hint: You can use the following function toUpper (having imported Data.Char):
capitalize :: String -> String
capitalize xs = [toUpper x | x <- xs]

-- 11 - Using a list comprehension, write a function sigma that calculates the sum
-- of i^2 | 1 <= i <= 100
sigma = sum [x^2 | x <- [1..100]]

-- 12 - Using a list comprehension,write a function sigma’
-- sigma’ :: Int-> Int
-- that takes an integer n and calculates -> i^2 | 1 <= i <= n
sigma' :: Int-> Int
sigma' n = sum [x^2 | x <- [1..n]]

-- 13 - Define the function
matches :: Integer -> [Integer] -> [Integer]
-- which picks out all occurences of an integer in a list. e.g.
-- > matches 1 [1,2,3,4,1]
--  [1,1]
matches x xs = [x' | x' <- xs, (x' == x)]

-- 14 - Suppose that a coordinate grid of size m x n is given by the list of all
-- pairs (x, y) of integers such that 0 ≤ x ≤ m and 0 ≤ y ≤ n. Using a list
-- comprehension, define a function:
grid :: Int -> Int -> [(Int, Int)]
-- that returns a coordinate grid of a given size. For example:
-- > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 15 - Using a list comprehension and the function grid above, define a function
square :: Int -> [(Int, Int)]
-- that returns a coordinate square of size n, excluding the diagonal from (0, 0) to
-- (n, n). For example:
-- > square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
square x = [(x',y) | (x',y) <- grid x x , not(x'== y)]

-- 16 - In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a]
-- that produces a list of identical elements can be defined using list comprehension.
-- (Call your version myReplicate) For example:
-- > myReplicate 3 True 
-- [True,True,True]
myReplicate x y = [y | _ <- [1..x]]

-- 17 - A triple (x, y, z) of positive integers is called pythagorean if x2 + y2 = z2.
-- Using a list comprehension, define a function
pyths :: Int -> [(Int,Int,Int)]
-- that returns a list of all such triples whose components are at most a given limit.
-- For example
-- > pyths 10 
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths x = [(p,q,r) | p <- [1..x], q <- [1..x], r <- [1..x], (p^2 + q^2)==r^2]

-- A positive integer is perfect if it equals the sum of all of its factors, excluding
-- the number itself. Using a list comprehension and the function factors, 
-- define a function:
-- perfects :: Int -> [Int]
-- that returns the list of all perfect numbers up to a given limit. For example:
-- > perfects 500 
-- [6,28,496]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects x = [x' | x' <- [1..x], (sum (tail (reverse (factors x')))) == x']
