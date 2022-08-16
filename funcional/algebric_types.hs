-- ! Tipos Algébricos
    -- Criar novos tipos de dados -> keyword "data"
    -- Usamos normalmente como qualquer tipo
    -- ? Muitas vezes precisamos criar uma instancia nova de Show e outras classes de tipo para ter mais usabilidades no tipo -> keyword deriving
    -- * Construtores (Rotulo)
        -- O nome nao precisa ser diferente do nome do tipo
        -- Sempre que crio um novo tipo de dados, preciso do construtor
        -- ? No codigo, para que o interpretador compreenda que eh o tipo que estamos usando, usamos o construtor para diferenciar
        -- * Parametros
            -- Constr param, param2...
            -- func (Pessoa name age) = age .... => func :: Pessoas -> Idade
    -- * Enum
        -- data Month = Jan | Fev | Mar | ... | Dez
        -- Cada nome eh um construtor mas nao tem parametro

data Month = Jan | Fev | Mar | Abr | Mai | Jun | Jul
    deriving (Show, Eq) -- Criando a instancia de show para poder transformar o tipo para string e Eq para usarmos operacoes de comparar etc

data Shape = Circle Float | Rec Float Float
    deriving (Show, Eq)

area :: Shape -> Float -- Casamento de padrao de acordo com o construtor
area (Circle radius) = pi * (radius^^2)
area (Rec b h) = b * h

-- ? Chamando: area (Circle 12)

-- ! Show Exp
data Expr = Lit Int |
    Add Expr Expr |
    Sub Expr Expr
    deriving(Show)

showExp :: Expr -> String
showExp (Lit num) = show num
showExp (Add exp1 exp2) = "( " ++ showExp exp1 ++ " + " ++ showExp exp2 ++ " )"
showExp (Sub exp1 exp2) = "( " ++ showExp exp1 ++ " - " ++ showExp exp2 ++ " )"

-- ! Lista Linkada
data List t = Nil | Cons t (List t) -- dois construtores, dependedo de qual usar, o casamento de padrao é outro
    deriving(Show, Eq)

createList :: List t -> [t]
createList Nil = []
createList (Cons elem rest) = elem : createList rest 
-- createList (Cons 2 (Cons 4 (Cons 10 Nil)))

-- ! Lista para Linkada
fromList :: [t] -> List t
fromList [] = Nil
fromList (head:tail) = Cons head (fromList tail)

-- ! Arvore