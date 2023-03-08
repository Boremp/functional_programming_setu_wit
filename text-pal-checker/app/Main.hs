module Main (main) where

import Palindrome

main :: IO ()
main = do
    input <- getLine
    let result  | isPalindromeT input = "It is a palindrome"
                | otherwise = "It is not a palindrome"
    print result
