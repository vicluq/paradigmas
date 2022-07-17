-- ! Lists
    -- Coleção de valores de _mesmo tipo_
    -- tipagem: [type]
    -- Sinonimo: String = [Char]
    -- [] é uma lista vazia de QUALQUER TIPO
    -- head (elemento) -> primeiro elemento 
    -- tail (lista) -> todo o resto da lista (se for o ultimo elemento, a tail eh a [])
    -- * Construtor de listas -> cabeça : []
        -- novaCabeça : antigaCabeca : [] (a cauda seria a lista vazia)
        -- Otimo para escrever listas recursivamente
    -- * Criando uma lista sequencial
        -- [start..end] -> [start, ..., end]
        -- [charStart..charEnd]
        -- Podemos dizer o step fornecendo o segundo numero, ele oega a diferenca e usa de step
            -- O padrão é ir de 1 em 1
    -- * Tamanho da lista
        -- length list 
    -- * Lista como paramentro da funcao
        -- func (head:tail) -> head eh o elemento da cabeça e tail eh o resto que sobra da lista 
        -- ? Desestruturação de lista
    -- * Concatenação
        -- list1 (++) list2
        -- (x:xs) ++ y = x : (xs ++ y)
    -- * Casamento de padrao
        -- func (h:[]) -> lista com so um elemento
        -- func [] -> lista vazia
        -- func (h:t) -> mais de um elemento
    -- * Muitas dessas são polimorficas
        -- varias instancias porem o compilador decide qual sera usada dependendo dos parametros e etc

-- ! Compreensão de Listas
    -- Forma de definir listas usando notação matematica
    -- [elem | elem <- [lista], condicao_booleana] -> elemento tal que elemento ta na lista tal (<-) segundo a condição

-- ! Case
    {-
        case (elem) of
            value1 -> return
            value2 -> return
            etc
        compara elem com os valores
    -}

-- ? Double
myList :: [Int]
myList = [1..10]

doubleList :: [Int] -> [Int]
doubleList [] = [] -- Caso base
doubleList (h:t) = (h*2) : doubleList t -- t é o resto da lista (cauda)

-- ? Member
member :: [Int] -> Int -> Bool

member [] num = False
member (h:t) num | h == num =True
                 | otherwise = member t num

-- ? Get string Digits
digits :: String -> String
digits [] = []
digits (charH:charT) | charH >= '0' && charH <= '9' = charH : digits charT
                     | otherwise = digits charT

-- ? Sum pairs of two lists
    -- casos: ambas vazias, uma vazia e a outra nao, as duas com coisa ainda
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs (h:t) [] = []
sumPairs [] (h:t) = []
sumPairs (h1:t1) (h2:t2) = (h1 + h2) : (sumPairs t1 t2)

-- ? Sum elements
sumElems :: [Int] -> Int
sumElems [] = 0
sumElems (h:t) = h + sumElems t

-- ? Maior da lista
maxList :: [Int] -> Int
maxList (h:[]) = h -- significa que so tem um elemento na lista
maxList (h:t) | h > maxList t = h
              | otherwise =maxList t  
