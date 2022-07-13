-- ! TUPLAS
    -- são ""listas"" de tamanho fixo na qual especificamos quantos e os tipos dos elementos
    -- ex: (Int, Bool) -> uma tupla de 2 alementos, 1 inteiro e o outro booleano
    -- * Definindo uma
        -- name :: (tupla)
        -- funcName :: type -> ... -> (tupla) -> ...
        -- funcName (tupla) ... = ...
        
-- ! Sinônimos de Tipos
    -- é o typedef
    -- type nameType = definition
    -- ex: type Person = (String, Int, String) -> nome, idade, cpf
    
type Point = (Float, Float)
type Segment = (Point, Point)

isVertical :: Segment -> Bool
isVertical ((x1, y1), (x2, y2)) = x1 == x2 

