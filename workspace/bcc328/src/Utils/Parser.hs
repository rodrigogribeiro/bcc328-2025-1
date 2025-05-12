module Utils.Parser ( module Utils.Parser
                    , module Control.Applicative 
                    ) where 

import Control.Applicative
import Data.Char 

-- basic parsers 

sat :: (s -> Bool) -> Parser s s
sat p = Parser (\ ts -> case ts of 
                          [] -> [] 
                          (t' : ts') -> 
                            if p t' then [(t', ts')] 
                                    else [])

token :: Eq s => [s] -> Parser s [s]
token s = Parser (\ ts -> if (take (length s) ts) == s 
                          then [(s, drop (length s) ts)]
                          else [])

-- end by 

endBy :: Parser s a -> Parser s b -> Parser s [a]
p `endBy` e 
  = many (f <$> p <*> e) 
    where 
      f x _ = x 

-- choice 

choice :: [Parser s a] -> Parser s a 
choice = foldr (<|>) empty

-- optional 

option :: Parser s a -> a -> Parser s a 
option p v = p <|> pure v 

-- parsing numbers 

digit :: Parser Char Int 
digit 
  = f <$> sat isDigit 
    where 
      f c = ord c - ord '0'

natural :: Parser Char Int 
natural 
  = foldl f 0 <$> many digit  
    where 
      f a b = a * 10 + b 

{-
 - foldl :: (b -> a -> b) -> b -> [a] -> b 
 - foldl _ v [] = v 
 - foldl f v (x : xs) = foldl f (f v x) xs 
 -
 - step a b = a * 10 + b 
 -
 - foldl step 0 [1,2,3] = 
 - foldl step (step 0 1) [2,3] = 
 - foldl step (0 * 10 + 1) [2,3] = 
 - foldl step 1 [2,3] = 
 - foldl step (step 1 2) [3] = 
 - foldl step (1 * 10 + 2) [3] = 
 - foldl step 12 [3] = 
 - foldl step (step 12 3) [] = 
 - foldl step (12 * 10 + 3) [] =
 - foldl step 123 [] =
 - 123
 -
 - -}

integer :: Parser Char Int 
integer = option (const negate <$> token "-") id <*> natural

-- greedy operator 

greedy :: Parser s a -> Parser s [a]
greedy p = first (many p)

first :: Parser s a -> Parser s a 
first (Parser p) = Parser (\ ts -> case p ts of 
                                     (x : _) -> [x]
                                     _ -> [])

-- chain operators 

chainl :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
chainl op p 
  = applyAll <$> p <*> many (flip <$> op <*> p)
    where 
      applyAll x [] = x
      applyAll x (f : fs) = applyAll (f x) fs

{- e + e + e + e está em e (+ e)*
 -
 -
 -
 -
 - }


-- definition of a simple parser 
-- infrastructure 

newtype Parser s a 
  = Parser { runParser :: [s] -> [(a, [s])] }

instance Functor (Parser s) where 
  fmap f (Parser p) = Parser g 
    where g ts = [(f x, ts') | (x, ts') <- p ts]

instance Applicative (Parser s) where 
  pure x = Parser (\ ts -> [(x,ts)])
  (Parser pf) <*> (Parser px)
    = Parser (\ ts -> [(f x, ts') | (f, ts1) <- pf ts
                                  , (x, ts') <- px ts1])

instance Alternative (Parser s) where 
  empty = Parser (\ _ -> [])
  (Parser p1) <|> (Parser p2) = Parser f 
    where f ts = p1 ts ++ p2 ts


