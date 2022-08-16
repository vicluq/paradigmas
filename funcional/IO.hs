import Set ( isEqual, makeSet, Set, isSub ) -- ! Importando modulos e seu conteudo (nome com maiuscula e devemos informar o conteudo)

set1 :: Set Integer
set1 = makeSet [1, 2, 3, 4]

set2 :: Set Integer
set2 = makeSet [1, 2, 3, 4]

main :: IO() -- Faz entrada e saida no console, por conta do tipo IO 
main = do 
       putStrLn (show (isEqual set1 set2)); -- print chama o show automaticamente, o putStrLn não
       print (if (isEqual set1 set2) then "Sao iguais" else "Nao sao iguais");
       str1 <- getLine;
       str2 <- getLine;
       print (if str1 == str2 then "Strings iguais" else "Strings NAO sao iguais");
       s1 <- getLine; -- ! Pegando uma linha do input -> retorna uma string
       s2 <- getLine;
       let set1 = read s1 :: Set Integer; -- ! Convertendo para tipos especificos quando lido do input (read)
       let set2 = read s2 :: Set Integer;
       print (if (isEqual set1 set2) then "Sao iguais" else "Nao sao iguais");
       print "refazer?"
       quest <- getLine
       if (quest == "Y") then main else return()


-- IO é da classe Monad -> classe com elementos para sequenciamento de ações

-- ! >> (encadeia operações IO, equivale ao 'do') e >>= (como se fosse o then de JS, recebe uma função cujo parametro dela é o retorno da que chama o >>=)
-- main :: IO()
-- main = print "Digite uma palavra: " >> getLine >>= \str -> putStrLn (show str)
