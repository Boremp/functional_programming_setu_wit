{-# LANGUAGE OverloadedStrings #-}
module Palindrome (isPalindrome, isPalindromeT) where

import Data.Char (isAlpha, toLower)
import qualified Data.Text as T

isPalindrome :: String -> Bool
isPalindrome xs = word == reverse word
                where 
                    word = map toLower $ filter isAlpha xs

isPalindromeT :: String -> Bool
isPalindromeT xs = word == T.reverse word
                where
                    word = T.toLower $ T.filter isAlpha $ T.pack xs