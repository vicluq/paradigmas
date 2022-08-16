module Set (makeSet, isEqual, isSub, Set) where -- ! Devemos indicar o que estamos exportando

data Set t = Set [t] -- Construtor Set recebe uma lista de elem do tipo t
  deriving (Show, Eq, Read)

-- deriving Eq é igual a isso de baixo
-- instance Eq t => Eq (Set t) where -- tipo o where nas funcoes (igual principio)
--     (Set []) == (Set []) = True
--     (Set (h1:t1)) == (Set (h2:t2)) = (h1 == h2) && (t1 == t2)
--     _ == _ = False

-- * Igualdade de conjuntos 

-- 1 elimina as duplicatas
-- 2 checa se os elementos sao os mesmos

makeSet :: [t] -> Set t
makeSet list = Set list

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (h:t) = h : removeDuplicates [x | x <- t, x /= h]

isPresent :: Eq t => t -> [t] -> Bool
isPresent elem [] = False
isPresent elem (h2:t2) | elem == h2 = True
                       | otherwise = False || isPresent elem t2 

isSub :: Eq t => [t] -> [t] -> Bool
isSub [] list2 = True
isSub (h1:t1) list2 = isPresent h1 list2 && isSub t1 list2

isEqual :: Eq a => Set a -> Set a -> Bool
isEqual (Set list1) (Set list2) = isSub (removeDuplicates list1) (removeDuplicates list2) && isSub (removeDuplicates list2) (removeDuplicates list1)