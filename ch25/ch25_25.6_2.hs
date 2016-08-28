newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (\x -> foldMap f x) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Compose fga) = Compose <$> f fga