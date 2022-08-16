-- ! Função de Alta Ordem
  -- Funções que recebem funções de argumento
  -- ? Para tipar -> Bota a tipagem da função que ela recebe (parenteses) e as dos outros parametros
  -- Função Map, Filter, Fold

-- * Map (JS)

map2 :: (t -> u) -> [t] -> [u]
map2 f [] = []
map2 f (h : t) = (f h) : map2 f t

-- * Fold / Reduce

fold2 :: (t -> t -> t) -> [t] -> t
fold2 f [h] = h
fold2 f (h : t) = f h (fold2 f t)

-- Ex: somar tudo da lista: fold2 (+) [...]

-- * Foldr (permite o elemento neutro, para quando chegarmos na lista vazia)

-- * Filter (JS)

filter2 :: (t -> Bool) -> [t] -> [t]
filter2 func [] = []
filter2 func (h : t)
  | func h = h : filter2 func t
  | otherwise = filter2 func t

-- ? Exercicio - determinar se a função é crescente
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent func 1 = func 1 > func 0
isCrescent func n = (func n > func (n -1)) && isCrescent func (n -1)

-- ? takeWhile e dropWhile
takeWhile2 :: Eq t => (t -> Bool) -> [t] -> [t]
takeWhile2 func (h : t)
  | func h = h : takeWhile2 func t
  | otherwise = []

dropWhile2 :: (t -> Bool) -> [t] -> [t]
dropWhile2 func (h:t) | func h = dropWhile2 func t
                      | otherwise = (h : t)