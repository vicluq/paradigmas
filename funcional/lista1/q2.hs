mul2 :: [Int] -> [Int] -> [Int]

mul2 [] [] = []
mul2 (h1:t1) [] = h1 * 0 : mul2 t1 []
mul2 [] (h2:t2) = h2 * 0 : mul2 [] t2
mul2 (h1:t1) (h2:t2) = h1 * h2 : mul2 t1 t2

main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result