module MyParser.Combinators
(
      many1
    , many
    , skipMany
    , sepBy

)
where

import MyParser

import Control.Applicative (Alternative (..))

many1 :: MyParser a -> MyParser [a]
many1 p = (:) <$> p <*> many p

skipMany :: MyParser a -> MyParser ()
skipMany p = toUnit <$> many p

sepBy p sep = MyParser $ fun []
  where

    fun parsed str = case apply p str of
      Left err          -> Right (parsed, str)
      Right (res, "") -> Right (res:parsed, "")
      Right (res, str') -> case apply sep str' of
        Left err'        -> Right (res:parsed, str')
        Right (_, str'') -> fun (res:parsed) str''


toUnit :: a -> ()
toUnit _ = ()
