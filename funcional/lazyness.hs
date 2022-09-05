-- ! Lazyness
    -- A avaliação de uma expressão só é feita se seu resultado for necessário para a execução da função
    -- * Ele nao avalia os parametros antes, primeiro ve o que a função usa, depois avalia os param usados (LAZY)
        -- Direita pra esquerda
        -- Também não avalia parametros iguais mais de uma vez -> se avalia um e usa no outro valor
    -- A avaliação estrita faz esquerda para a direita
    -- ? Exemplo da lista infinita f (a:t) (b:t2) = a + b
    -- ! Recurso de cauda
        -- Na normal, a ultima chamada (que retorna o valor final), antes de retornar, realiza todas as operações com todos os retornos das outras chamadas
        -- Na de cauda, a cada chamada, retornamos o valor atual ja calculado, de forma que na ultima chamada, ela só retorne sem realizar grandes operações, ao inves de resolvr cada chamada anterior
        -- Isso evita a necessidade de empilhr stack frames uma vez que aquela ja foi concluida

crivo :: [Int] -> [Int]
crivo [] = []
crivo (h:t) = h : crivo [x | x <- t, x `mod` h /= 0]
-- adiciona o elemento (o primo que sobrou do crivo junto com o crivo da lista)

splitWords :: String -> [String]
splitWords [] = [""] -- Caso base: string vazia
splitWords (' ' : cs) = "" : splitWords cs -- Concatena a vazia que sera reenchida pela prox palavra
splitWords (ch:cs) = (ch : head (splitWords cs)) : tail (splitWords cs)
-- Concatena o caracter com a posição do head atual (string mntada)