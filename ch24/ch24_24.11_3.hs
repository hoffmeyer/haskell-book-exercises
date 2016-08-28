import Text.Trifecta
import Data.Char (digitToInt)
import Control.Applicative ((<|>))

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" 

joinNumber :: [Char] -> Integer
joinNumber = toInteger . foldl addThem 0
  where addThem acc d = acc * 10 + digitToInt d

parseSign :: Parser Bool
parseSign = try (char '-' >> return True) <|> return False

base10Integer :: Parser Integer
base10Integer = do
  isNegative <- parseSign
  digits <- some parseDigit
  let n = joinNumber digits
  return (if isNegative then (- n) else n)

main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10Integer mempty "-123abc"
