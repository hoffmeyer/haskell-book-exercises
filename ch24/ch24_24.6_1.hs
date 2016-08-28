{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where
import Control.Applicative
import Data.Ratio ((%), Ratio(..))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type NumberOrFraction =
  Either Integer (Ratio Integer)

parseNR :: Parser NumberOrFraction
parseNR =
      (Right <$> try virtuousFraction)
  <|> (Left <$> try integer)

main :: IO ()
main = do
  print $ parseString parseNR mempty badFraction
  print $ parseString parseNR mempty alsoBad
  print $ parseString parseNR mempty shouldWork
  print $ parseString parseNR mempty shouldAlsoWork