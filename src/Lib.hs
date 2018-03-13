module Lib
    (
        MyParser

      , anyChar

    ) where

import Data.Char (digitToInt, isDigit, isLower)

newtype MyParser a = MyParser {
      apply :: String -> Either String (a, String)
}

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

instance Functor MyParser where
    -- | fmap :: (a -> b) -> MyParser a -> MyParser b
    fmap f p = MyParser fun where
      fun inp = mapFst f <$> (apply p) inp


instance Applicative MyParser where
    pure a = MyParser $ \str -> Right (a, str)
    fp <*> dp = MyParser fun where
      fun str = case apply fp str of
        Left err        -> Left err
        Right (f, str') ->  apply (f <$> dp) str'

anyChar :: MyParser Char
anyChar = satisfy true

lower :: MyParser Char
lower = satisfy isLower

char :: Char -> MyParser Char
char c = satisfy (==c)

digit :: MyParser Int
digit = digitToInt <$> satisfy isDigit

satisfy :: (Char -> Bool) -> MyParser Char
satisfy = satisfy' "Char does not match the predicate"

true :: a -> Bool
true _ = True

satisfy' :: String -> (Char -> Bool) -> MyParser Char
satisfy' err pr = MyParser fun where
  fun "" = Left "Input string is empty"
  fun (c:cs) | pr c = Right (c, cs)
             | otherwise = Left err
