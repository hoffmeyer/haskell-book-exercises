separate :: Char -> [Char] -> [[Char]]
separate _ [] = []
separate sep xs = case dropWhile (== sep) xs of
                      [] -> []
                      az -> w : separate sep rest
                            where (w, rest) = break (== sep) az

-- using purely takeWhile and dropWhile
separate' :: Char -> [Char] -> [[Char]]
separate' _ [] = []
separate' sep xs = w : separate' sep ws
  where w = takeWhile (/= sep) xs
        ws = dropWhile (== sep) (drop (length w) xs)

myWords :: [Char] -> [[Char]]
myWords = separate ' '


myLines :: String -> [String]
myLines = separate '\n'
