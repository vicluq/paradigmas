getAllBigger :: [Int] -> Int -> [Int]

getAllBigger [] pivot = [] 
getAllBigger (h:t) pivot | h >= pivot = h : getAllBigger t pivot
                         | otherwise = getAllBigger t pivot

getAllSmaller [] pivot = []
getAllSmaller (h:t) pivot | h < pivot = h : getAllSmaller t pivot
                          | otherwise = getAllSmaller t pivot

quickSort :: [Int] -> [Int] -- Lembrar que o pivot acara sendo o elemento do meio pq vou jogando os menores na esquerda e os maiores pela direita
quickSort [] = []
quickSort (h : t) = quickSort (getAllSmaller t h) ++ [h] ++ quickSort (getAllBigger t h)
-- Quando chegaros na recursao onde so tem um elemento da lista, ele eh o pivot e precisamos coloca-lo no meio pq getAllBiugger e getAllSmaller retornara vazio