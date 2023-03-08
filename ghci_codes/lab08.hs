-- EXERCISES ON HIGHER ORDER FUNCTIONS

-- 1 - Show how the list comprehension
-- [f x | x <- xs, p x]
-- can be re-expressed using the higher-order functions map and filter 
-- R: map f $ filter p xs

-- *** 2 - Without looking at the definitions from the standard prelude, define the
-- following higher-order library functions on lists.

-- 2.1 -Decide if all elements of a list satisfy a predicate:
all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) | f x = all' f xs
              | otherwise = False
            
-- 2.2 - Decide if all elements of a list satisfy a predicate:
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) | f x = True
              | otherwise = any' f xs

-- 2.3 - Select elements from a list while they satisfy a predicate:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

-- 2.4 - Remove elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = (x:xs)

-- **** 3 - Redefine the functions "map f" and "filter p" using foldr
{- R:  
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f v [] = v
    foldr f v (x:xs) = f x (foldr f v xs)
-}
