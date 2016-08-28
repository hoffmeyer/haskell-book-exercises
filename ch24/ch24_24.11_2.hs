import Text.Trifecta
import Data.Char (digitToInt)

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" 

joinNumber :: [Char] -> Integer
joinNumber = toInteger . foldl addThem 0
  where addThem acc d = acc * 10 + digitToInt d

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  return (joinNumber digits)

main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
