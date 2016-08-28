module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof >> stop

one' = one >> stop

oneTwo = char '1' >> char '2' >> eof >> stop

oneTwo' = oneTwo >> stop

oneStr = string "1"  >> stop

oneTwoStr = string "12"

oneTwoThreeStr = string "123"

oneTwoThreeChar = char '1' >> char '2' >> char '3'

stringChar (x:xs) = char x >> stringChar xs
stringChar [] = stop

oneTwoThreeChar' = stringChar "123"

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p =
  print $ parseString p mempty "123"

testParse'' :: Show a => Parser a -> IO ()
testParse'' p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneStr':"
  testParse' oneStr
  pNL "oneTwoStr':"
  testParse' oneTwoStr
  pNL "oneTwoThreeStr':"
  testParse'' oneTwoThreeStr
  pNL "oneTwoThreeChar:"
  testParse oneTwoThreeChar
  pNL "oneTwoThreeChar':"
  testParse oneTwoThreeChar'
