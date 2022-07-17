-- Split between spaces
splitSpaces :: String -> [String]
splitSpaces str = words str

-- Remove semicolon and create tuples
createTuple :: [String] -> [(String, String, Double)] -- ["JAN", "Amazon", "40.32"] = ()
createTuple [] = []
createTuple (day : month : origin : value : tail) = (month, origin, read value :: Double) : createTuple tail

removeSemicolon :: String -> [(String, String, Double)]
removeSemicolon str = createTuple (words [if i == ';' then ' ' else i | i <- str])

parseString :: String -> [(String, String, Double)]
parseString str = removeSemicolon str

-- Getting the result
getMonthExpenses :: [(String, String, Double)] -> String -> Double
getMonthExpenses [] month = 0
getMonthExpenses (("JAN", origin, value): tail) month = value + getMonthExpenses tail month
getMonthExpenses ((m, origin, value): tail) month = getMonthExpenses tail month


logMes :: String -> String -> Double
logMes str month = getMonthExpenses (parseString str) month

-- main = do
--     a <- getLine
--     b <- getLine
--     let result = logMes a b
--     print result