module Palindrome (isPalindrome) where

import Data.Char (isAlpha, toLower)

isPalindrome :: [Char] -> Bool
isPalindrome xs = word == (reverse word)
                where 
                    word = map toLower (filter (isAlpha) xs)