-- ! Lazyness
    -- A avaliação de uma expressão só é feita se seu resultado for necessário para a execução da função
    -- * Ele nao avalia os parametros antes, primeiro ve o que a função usa, depois avalia os param usados (LAZY)
        -- Direita pra esquerda
    -- A avaliação estrita faz esquerda para a direita
    -- ? Exemplo da lista infinita f (a:t) (b:t2) = a + b

crivo :: [Int] -> [Int]
crivo [] = []
crivo (h:t) = h : crivo [x | x <- t, x `mod` h /= 0]
-- adiciona o elemento (o primo que sobrou do crivo junto com o crivo da lista)