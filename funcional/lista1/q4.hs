findAndReplaceChar :: Char -> [(Char, Char)] -> Char
findAndReplaceChar char [] = char
findAndReplaceChar char ((ch, key) : tail)
  | ch == char = key
  | otherwise = findAndReplaceChar char tail

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] keys = []
decEnigma (char : tail) keys = findAndReplaceChar char keys : decEnigma tail keys