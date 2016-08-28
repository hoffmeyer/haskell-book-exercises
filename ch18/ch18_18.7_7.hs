import Control.Monad (join)


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

main = do
  print $ flipType [Just 2, Just 5, Just 10]
