-- ! Polimorfismo
    -- * Logo, são funções que funcionam para diferentes tipos de dados
    -- ? Monomorficas -> funcionam para apenas um tipo de dado
    -- * Tipos genéricos (variáveis de tipos)
        -- Polimorfismo Paramétrico (tipo é um parametro)
    -- Haskell tem inferencia de tipo, logo em alguns casos em que so iremos lidar com um tipo, nao precisamos especificar
        -- Ou em casos que nao envolvam operações, etc
    -- ? Descobrindo o tipo no interpretador => :type elem

-- ! Classes de tipos
    -- Classes que representam um conjunto de tipos (tipos numeros, etc)
    -- As insgtancias delas sao TIPOS. a classe Eq por exemplo tem varias instancias -> Int, Float, Char
    -- ? Criando um tipo com isso
        -- ex: Eq t => [t] -> Bool
            -- t é uma instancia de Eq (um tipo dessa classe) e a função retorna um bool
    -- * Classes
        -- Show -> tudo que é convertivel pra string
        -- Read -> tudo que é convertivel de uma string para um tipo especificado
        -- Integral
        -- Fractional

-- ! Overloading/Sobrecarga
    -- Reusamos o mesmo nome de função e reescrevemos a função adaptada (definimos para pram/tipos distintos)
    -- O compilador infere qual deve ser a sobrecarga executada (baseada nos parametros e tipos)

join :: [t] -> [u] -> [(t, u)] -- t e u são tipos genericos
join [] [] = []
join [] (h:t) = []
join (h:t) [] = []
join (h1:t1) (h2:t2) = (h1, h2) : join t1 t2 

-- ? Exercicio: Concatena os arrays e conta
concatLists :: [[t]] -> [t]
concatLists [] = []
concatLists (list:tail) = list ++ concatLists tail

countElement :: Eq t => t -> [t] -> Int
countElement elem [] = 0
countElement elem (h:t) | elem == h = 1 + countElement elem t
                        | otherwise = countElement elem t

countForList :: Eq t => [t] -> [(t, Int)] -- Pego um desse pra cada lista e depois monto uma lista geral
countForList [] = []
countForList (h:t) = (h, (countElement h t) + 1) : countForList [x | x <- t, x /= h]


agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar elem = countForList (concatLists elem)