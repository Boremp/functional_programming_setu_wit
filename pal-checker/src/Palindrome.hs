module Palindrome (isPalindrome) where

isPalindrome :: [Char] -> Bool
isPalindrome xs = xs == (reverse xs)
               