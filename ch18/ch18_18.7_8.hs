import Control.Monad (join)


meh :: Applicative m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

flipType :: (Applicative m) => [m a] -> m [a]
flipType xs = meh xs id

main = do
  print $ flipType [Just 2, Just 5, Just 10]
