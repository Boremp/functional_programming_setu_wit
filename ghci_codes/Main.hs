main = print (fac 20)

fac 0 = 1
fac n = n * fac (n-1)

{-
responseFor :: String -> String
responseFor [] = "Fine. Be that way!"
responseFor xs  | isQuestion && isYell = "Calm down, I know what I'm doing!"
                | isQuestion = "Sure."
                | isYell = "'Whoa, chill out!"
                | otherwise = "Whatever."
                where 
                    isQuestion = '?' `elem` xs 
                    isYell = all (isUpper) xs
-}


{-
    x -> para numeros (n)
    xs -> para lista de numeros (ns)
-}
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns