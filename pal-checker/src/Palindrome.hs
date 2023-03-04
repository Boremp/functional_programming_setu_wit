module Palindrome (isPalindrome) where

isPalindrome :: [Char] -> String
isPalindrome xs | xs == (reverse xs) = "It is a palindrome"
                | otherwise = "It is not a palindrome"