newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

