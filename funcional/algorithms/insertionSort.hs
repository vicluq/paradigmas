insert :: Ord t => t -> [t] -> [t]
insert elem [] = [elem]
insert elem (h:t) | elem < h = elem : h : t
                  | otherwise = h : insert elem t

is :: [Int] -> [Int]
is [] = []
is (h:t)  = insert h (is t) -- Insiro h numa lista ordenada, e a forma de ordenar a lista eh chamando o insertion sort na calda ate chegarmos num elemento atomico e a recursao add um a um