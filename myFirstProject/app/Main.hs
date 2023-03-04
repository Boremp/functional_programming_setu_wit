module Main (main) where

import Split.LibSplit

main :: IO ()
main = do
    input <- getLine
    let result = ourSplit input
    print result
