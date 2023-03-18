module Main (main) where

import qualified Data.Text as T
import Utils (stringtoTitle, toTitle)

main :: IO ()
main = do
    input <- getLine
    let title = toTitle $ T.pack input
    print title
