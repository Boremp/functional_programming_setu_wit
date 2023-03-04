module Palindrome (isPalindrome) where

import Data.Char (isAlpha, toLower)

isPalindrome :: [Char] -> String
isPalindrome xs | word == (reverse word) = "It is a palindrome"
                | otherwise = "It is not a palindrome"
                where 
                    word = map toLower (filter (isAlpha) xs)