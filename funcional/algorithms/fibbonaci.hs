fibbonaci :: Int -> Int
fibbonaci 1 = 1
fibbonaci 2 = 1
fibbonaci n = fibbonaci (n -1) + fibbonaci (n -2)

listFib :: Int -> [Int]
listFib 1 = [1]
listFib n = listFib (n -1) ++ [fibbonaci n]

pairFibbonaci :: Int -> Int -> Int -> [Int]
pairFibbonaci n count current
  | count == n = []
  | even (fibbonaci current) = fibbonaci current : pairFibbonaci n (count + 1) (current + 1)
  | otherwise = pairFibbonaci n count (current + 1)