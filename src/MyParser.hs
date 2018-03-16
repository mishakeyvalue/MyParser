module MyParser
  (
        MyParser (..)
  )

where

import Control.Applicative (Alternative (..))

newtype MyParser a = MyParser {
      apply :: String -> Either String (a, String)
}

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

instance Functor MyParser where
    fmap f p = MyParser fun where
      fun inp = mapFst f <$> (apply p) inp


instance Applicative MyParser where
    pure a = MyParser $ \str -> Right (a, str)
    fp <*> dp = MyParser fun where
      fun str = case apply fp str of
        Left err        -> Left err
        Right (f, str') ->  apply (f <$> dp) str'

instance Alternative MyParser where
  empty = MyParser f where
    f _ = Left "empty"

  lp <|> rp = MyParser fun where
    fun str = case apply lp str of
      Left _  -> apply rp str
      Right d -> Right d
