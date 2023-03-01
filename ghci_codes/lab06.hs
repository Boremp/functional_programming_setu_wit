-- Exercises Recursive Functions

-- The stars (*) indicate the level of difficulty

-- ** 1 - Define a recursive function
sumdown :: Int -> Int
-- that returns the sum of all the non-negative integers down to zero. For example:
-- >sumdown 3
-- will return 3 + 2 + 1 + 0 = 6
sumdown 0 = 0
sumdown x = x + sumdown(x-1)

-- ** 2 - Define the exponention (to the power of) function for non-negative numbers
-- using the same pattern of recursion as the multiplication operator in notes, and
-- show how the expression
exponention:: Int -> Int -> Int
exponention 0 _ = 0
exponention _ 0 = 1
exponention x y = x * exponention x (y-1)

-- ** 3 - Define a recursive function
-- f i b o n a c c i : : Int −> Int
-- that calculates the Fibonacci number as per the following definition
-- F0 = 0, F1 = 1
-- Fn = Fn−1 + Fn−2
fib:: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- fibonacci sequence list
fibList n = [fib x | x <- [0..n]]

-- ** 4 - Define a recursive function
-- that removes the last element from a non-empty list. Construct the definition
-- using the 5 step process as discussed in lectures.

-- step 1 define the type
myInit:: [a] -> [a]
-- step 2 Enumerate the cases -> [] & (x:xs)
myInit (_:[]) = [] -- step 3 define the simple case
myInit (x:xs) = x : myInit xs -- step 4 define the other cases
-- step 5 generalise and simplifly

-- ** 5 - Without looking at the definitions from the standard Prelude, define 
-- the following library functions on lists using recursion:
-- 5.1. Decide if all logical values in a list are True
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (False:_) = False
myAnd (_:xs) = myAnd xs

-- 5.2. Concatenate a list of lists
myConcat :: [[a]]-> [a]
myConcat (x:[]) = x
myConcat (x:xs) = x ++ myConcat xs 

-- 5.3. Produce a list with n identical elements
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x

-- 5.4. Select the nth element of a list
myNth :: [a]-> Int -> a
myNth (x:_) 0 = x 
myNth [] _ = error "INDEX IS OUT OF BOUNDS "
myNth (_:xs) n = myNth xs (n-1)

-- 5.5. Decide if an value is an element of a list
myElem :: Eq a => a -> [a] -> Bool
myElem n [] = False
myElem n (x:xs) | n == x = True
                | otherwise = myElem n xs

-- Class exercise
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- *** 6 - Using the five-step process, construct the library functions that:
-- 1. calculate the sum of a list of numbers;
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

-- 2. take a given number of elements from the start of a list;
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

-- 3. select the last element of non-empty list.
myLast :: [a] -> a
myLast [] = error "EMPTY LIST"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- *** 7 - Define a recursive function
merge :: Ord a => [a] -> [a] -> [a]
-- that merges two sorted lists to give a single sorted list. 
-- Note : Your definition should not use other functions on sorted lists such as insert or isort, but
-- should be defined using explicit recursion.
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : (merge xs (y:ys))
                    | otherwise = y : (merge (x:xs) ys)

-- **** 8 - Using merge, define a function
-- that implements merge sort, in which the empty list and singleton lists are already
-- sorted, and any other list is sorted by merging together the two lists that
-- result from sorting the two halves of the list separately.
halve :: [a] -> ([a],[a])
halve [x] = ([x],[])
halve xs = (fstHalf, sndHalf)
            where
                fstHalf = take half xs
                sndHalf = drop half xs
                half = (length xs)`div`2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
            where 
                (left, right) = halve xs

-- FINISHED

-- Exercises Tail Recursive Functions

sumI :: [Int] -> Int
sumI ns = helper ns 0 where 
            helper [] acc = acc
            helper (x:xs) acc = helper xs (acc + x)

addT :: (Eq t, Num t) => t -> t -> t
addT x 0 = x
addT 0 y = y
addT x y = addT (x-1) (y+1)

-- 1 - In the previous exercise set, you wrote the code for:
-- fibonacci :: Int -> Int
-- Now, write this fibonacci using tail recursion.
fibT :: Int -> Int
fibT n =  helper n 0 1 where
    helper 0 aux _ = aux
    helper 1 _ acc = acc
    helper x aux acc = helper (x-1) (acc) (aux + acc)

fibTList n = [fibT x | x <- [0..n]]

-- 2 - Write a function
multT :: Int -> Int -> Int
-- which takes two positive integers and returns the product of the two numbers.
-- This calcluation can only use + and −. Ensure that the solution is tail recursive
multT 1 y = y
multT x 1 = x
multT x y = helper x y 0 where
    helper 0 _ acc = acc
    helper _ 0 acc = acc
    helper x y acc = helper (x-1) y (acc+y)

-- 3 - Given the recursive function
-- reverse :: [ a ] −> [ a ]
-- reverse [ ] = [ ]
-- reverse ( x : xs ) = reverse xs ++ [ x ]
-- that returns the reverse of a list, but this time, using tail recursion.
reverseT :: [a] -> [a]
reverseT ns = helper ns [] where
    helper [] acc = acc
    helper (x:xs) acc = helper xs (x:acc)