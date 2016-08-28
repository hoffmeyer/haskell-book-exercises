import Text.Trifecta

parseReturnNumber :: Parser Integer
parseReturnNumber = do
  number <- decimal
  eof
  return number

main :: IO ()
main = do
  print $ parseString parseReturnNumber mempty "123"
  print $ parseString parseReturnNumber mempty "123abc"