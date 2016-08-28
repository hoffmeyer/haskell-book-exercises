import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Text.Trifecta


-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving Show

instance Eq NumberOrString where
  (==) (NOSI i) (NOSI i') = i == i'
  (==) (NOSS s) (NOSS s') = s == s'
  (==) _ _ = False

instance Ord NumberOrString where
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI _) _         = LT
  compare _        (NOSI _)  = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving Show

instance Eq SemVer where
  (==) (SemVer ma mi pa re me) (SemVer ma' mi' pa' re' me') = 
    ma == ma' && mi == mi' && pa == pa' && re == re' && me == me'

instance Ord SemVer where
  compare (SemVer ma mi pa re _) (SemVer ma' mi' pa' re' _) =
    let c = (compare ma ma') <> (compare mi mi') <> (compare pa pa')
    in case c of
        EQ -> compare re re'
        _  -> c

nos :: Parser NumberOrString
nos = do
  (NOSI <$> decimal) <|> (NOSS <$> some letter)

nosDot :: Parser NumberOrString
nosDot = do
  ns <- nos
  skipMany (oneOf ".")
  return ns

relP :: Parser [NumberOrString]
relP = do
  (char '-' >> many nosDot) <|> (return [])

metP :: Parser [NumberOrString]
metP = do
  (char '+' >> many nosDot) <|> (return [])

parseSemVer :: Parser SemVer
parseSemVer = do
  ma <- decimal
  char '.'
  mi <- decimal
  char '.'
  pa <- decimal
  rel <- try relP
  met <- try metP
  return $ SemVer ma mi pa rel met 
