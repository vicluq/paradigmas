-- ! Jeito 1
isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 chr = True -- Acabou a string e amount acabou
isReplica [] amount chr = False -- Acabou a string e ainda tem coisa pra contar
isReplica (ch : strT) amount chr
  | length strT + 1 > amount = False
  | ch /= chr = False
  | ch == chr = True && isReplica strT (amount - 1) chr

-- ! Jeito 2 (mais limpo)
-- createString :: Char -> Int -> String
-- createString char 0 = []
-- createString char amount = char : createString char (amount-1)

-- isReplica :: String -> Int -> Char -> Bool
-- isReplica str amount chr = str == createString chr amount

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  c <- getChar
  let result = isReplica a (read b) c
  print result