{-# LANGUAGE BlockArguments #-}

module Module_1601212536_92709 where
    
import Control.Applicative

ensemble :: Either [String] Integer
ensemble
    =   parser1
    <|> parser2
    <|> parser3

parser1 :: Either [String] b
parser1 = Left ["oops"]

parser2 :: Either [String] b
parser2 = Left ["bad"]

parser3 :: Either [String] Integer
parser3 = Right 99

instance (Monoid a) => Alternative (Either a) where 
  empty = Left mempty
  Right a <|> _ = Right a
  Left a <|> Right b = Right b
  Left a <|> Left b = Left (a <> b)
