import Data.Char (isDigit)

-- -- Split between spaces
-- splitSpaces :: String -> [String]
-- splitSpaces str = words str

-- -- Remove semicolon and create tuples
-- createTuple :: [String] -> (String, String, Double) -- ["14 JAN", "Amazon", "40.32"] = ()
-- createTuple (day : month : origin : value : []) = (day ++ month, origin, read value :: Double)

-- removeSemicolon :: String -> (String, String, Double)
-- removeSemicolon str = createTuple (words [if i == ';' then ' ' else i | i <- str])

-- parseLines :: [String] -> [(String, String, Double)]
-- parseLines [] = []
-- parseLines (str : tail) = removeSemicolon str : parseLines tail

-- -- Getting the result
-- getMaxValue :: [(String, String, Double)] -> Double
-- getMaxValue ((date, origin, value) : tail)
--   | value > getMaxValue tail = value
--   | otherwise = getMaxValue tail

-- getMinValue :: [(String, String, Double)] -> Double
-- getMinValue ((date, origin, value) : tail)
--   | value < getMinValue tail = value
--   | otherwise = getMinValue tail

-- parseString :: String -> [(String, String, Double)]
-- parseString str = parseLines (splitSpaces str)

-- minMaxCartao :: String -> (Double, Double)
-- minMaxCartao str = (getMaxValue (parseString str), getMinValue (parseString str))

-- Remove Semicolon
removeSpaces :: String -> String
removeSpaces str = [i | i <- str, i /= ' ']

removeSemicolon :: String -> [String]
removeSemicolon str = words [if i == ';' then ' ' else i | i <- (removeSpaces str)]

isValidNumber :: String -> Bool
isValidNumber [] = True
isValidNumber (h : t)
  | (h >= '0' && h <= '9') || h == '.' = isValidNumber t
  | otherwise = False

getOnlyFloats :: [String] -> [Double]
getOnlyFloats [] = []
getOnlyFloats (str : tail)
  | isValidNumber str = (read str :: Double) : getOnlyFloats tail
  | otherwise = getOnlyFloats tail

-- -- Getting the result
getMaxValue :: [Double] -> Double
getMaxValue (value : []) = value
getMaxValue (value : tail)
  | value > getMaxValue tail = value
  | otherwise = getMaxValue tail

getMinValue :: [Double] -> Double
getMinValue (value : []) = value
getMinValue (value : tail)
  | value < getMinValue tail = value
  | otherwise = getMinValue tail

minMaxCartao :: String -> (Double, Double)
minMaxCartao str = (getMinValue (getOnlyFloats (removeSemicolon str)), getMaxValue (getOnlyFloats (removeSemicolon str)))