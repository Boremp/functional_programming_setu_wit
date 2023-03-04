module Main (main) where

import Palindrome

main :: IO ()
main = do
    input <- getLine
    let result = isPalindrome input 
    print result
