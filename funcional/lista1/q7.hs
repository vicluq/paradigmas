addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos num = " " ++ addEspacos (num - 1)

paraDireita :: Int -> String -> String
paraDireita numSpaces str = addEspacos numSpaces ++ str

parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry paraDireita . parseInput