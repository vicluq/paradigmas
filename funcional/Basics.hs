-- notação: programacao é baseada em definicoes
-- nome_da_Var_ou_Elem :: type (definição)
-- nome_da_Var_ou_Elem = value (atribuição)

-- barra de divisao só funciona com Float
-- para Int, usamos "`div`"

answer :: Int
answer = 42

greater :: Bool
greater = (answer > 53)

-- ! Funções em Haskell
-- nome :: type_param_1 -> ...-> type_param_x -> type_retorno
-- nome lista_parametros = definição
-- Chamando: nome param_1 param_2 ...

squareRoot :: Float -> Float
squareRoot x = x * x

-- ! Condicionais (cada barrinha é um if. "otherwise" é o else)

maxNum :: Int -> Int -> Int
maxNum a b
  | a >= b = a
  | otherwise = b

-- EXERCICIO

imc :: Float -> Float -> Float
imc h w = w / (squareRoot h)

test :: Float -> Float -> Float -> Bool
test a b c
  | (a < b && b < c) = True
  | otherwise = False

-- ! Casamento de padrão (tipo um switch)
-- 	Se o parametro for tal valor, ele ja retorna algo (pra nao precisar fazer um if

vendas :: Int -> Int
vendas 1 = 25
vendas 2 = 30
vendas 3 = 25
vendas n = n + 1

-- ! Recursão (não temos loops, usaremos recursão)
somatorioVendas :: Int -> Int
somatorioVendas w
  | w == 0 = 0 -- caso base
  | w > 0 = somatorioVendas (w - 1) + (vendas w)

-- Outra forma (mais limpa)
-- somatorioVendas2:: Int -> Int
-- somatiorioVendas2 0 = 0 -- Caso base
-- somatorioVendas2 w = somatiorioVendas2(w - 1) + (vendas w)

-- Exercicio 2: total de semanas com o numero de vendas igual a S (FAZER COM A AUXILIAR conta_se_True)
getTotalEqualToS :: Int -> Int -> Int
getTotalEqualToS s n
  | n == 0 = 0
  | (vendas n == s) = (getTotalEqualToS s (n - 1)) + 1
  | (vendas n /= s) = getTotalEqualToS s (n -1)

-- Exercicio 3: se o numero é primo
checkPrime :: Int -> Int -> Bool
checkPrime x y
  | y == 1 = True
  | x `mod` y /= 0 = True && checkPrime x (y -1)
  | x `mod` y == 0 = False

isPrime :: Int -> Bool
isPrime x
  | x < 2 = False
  | otherwise = checkPrime x (floor (sqrt (fromIntegral x))) -- sqrt recebe double, deve converter

-- ! Notação com aspas de crase pra funções de 2 params "param1 `funcName` param2"
-- ! "Where" (local a definição da função) e "let e in"

sumSqrs :: Int -> Int -> Int
sumSqrs x y = sqrX + sqrY
  where
    sqrX = x * x
    sqrY = y * y

-- ! Caracteres -> tipo Char (biblioteca Data.char)
-- parecido com C

-- ! String
-- (++) -> concatena duas string
-- show elem -> converte o elemento pra string
-- putStr/putStrLn (podemos usar caracteres especiais e formatacao)
-- Comparações de tabela ascii
-- words str -> separa pelos espaços

-- ! Float e Double
-- ceiling, floor, round
-- fromIntegral (converte de Int pra Float)

-- * Exercicio -> imprimir tabela

imprimeVendas :: Int -> Int -> String
imprimeVendas semana n
  | semana > n = ""
  | semana <= n =
    "  " ++ show semana ++ "        " ++ show (vendas semana) ++ "\n" -- Converter pra função
      ++ imprimeVendas (semana + 1) n

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ "      " ++ show (somatorioVendas n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Media" ++ "      " ++ show (fromIntegral (somatorioVendas n) / fromIntegral n) ++ "\n"

cabecalho :: String
cabecalho = "Semana   Venda\n"

imprimeTabela :: Int -> String
imprimeTabela n =
  cabecalho
    ++ imprimeVendas 1 n
    ++ imprimeTotal n
    ++ imprimeMedia n
