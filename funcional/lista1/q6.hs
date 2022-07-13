getBinPow :: String -> Int -> Int
getBinPow [] currPow = 0 -- Acabou as potencias
getBinPow (num:tail) currPow | num == '1' = 2^currPow + getBinPow tail (currPow - 1)
                             | num == '0' = 0 + getBinPow tail (currPow - 1)


btoi :: String -> Int
btoi binNumber = getBinPow binNumber (length binNumber - 1)

main = do
    s <- getLine
    let result = btoi s
    print result