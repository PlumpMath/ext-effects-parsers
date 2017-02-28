{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsers where

import Control.Monad
import Control.Applicative(pure,(<$>))
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Exception
import Data.Maybe
import Data.Char

type Fail = Exc ()

die :: (Member Fail r) => Eff r a
die = throwError ()

-- | Parser effects constraint
type Parsable r = (Member Fail r, Member (State String) r)

type Parser r a = Parsable r => Eff r a

-- | Run a computation with Fail and State String effects, yielding last state
--   in case of fail
parse :: Eff (Fail ': State String ': '[]) a -> String -> (Either () a, String)
parse p inp = run $ runState (runError p) inp

-- | Try to apply parser, in case of fail, backtrack and apply second one
alt :: Parser r a -> Parser r a -> Parser r a
alt ma mb = do
  s <- get @String
  catchError ma $ \(ea :: ()) -> do
    put s
    catchError mb $ \(eb :: ()) -> throwError ()

-- | Consumes one symbol of any kind
item :: Parser r Char
item = do
  s <- get
  case s of
    [] -> put s >> throwError ()
    (x:xs) -> put xs >> pure x

-- | Consumes item only if it satisfies predicate
sat :: (Char -> Bool) -> Parser r Char
sat p = do
  s <- get @String
  x <- item
  if p x then pure x else (put s >> throwError ())

-- | Consumes item only if it is equal to specified char
char :: Char -> Parser r Char
char x = sat (\y -> x == y)

lower :: Parser r Char
lower = sat isLower

upper :: Parser r Char
upper = sat isUpper

letter :: Parser r Char
letter = lower `alt` upper

digit :: Parser r Char
digit = sat isDigit

alphanum :: Parser r Char
alphanum = letter `alt` digit

-- | Parse a thing enclosed in brackets
bracket :: Eff r a -> Eff r b -> Eff r c -> Eff r b
bracket open p close = do
  open
  x <- p
  close
  return x

-- | Apply parser zero or more times. Similar to @many@ from Control.Applicative
many :: Parser r a -> Parser r [a]
many v = many_v
 where
   many_v = some_v `alt` (pure [])
   some_v = (fmap (:) v) <*> many_v

-- | Apply parser one or more times. Similar to @some@ from Control.Applicative
some :: Parser r a -> Parser r [a]
some v = some_v
 where
   many_v = some_v `alt` pure []
   some_v = (fmap (:) v) <*> many_v

word :: Parser r String
word = some letter

-- | Parse a sequence of entities with first parser, separated by things
--   parsable with second one
sepby :: Parser r a -> Parser r b -> Parser r [a]
p `sepby` sep = (p `sepby1` sep) `alt` return []

-- | Same as @sepby@, but sequence must be non-empty
sepby1 :: Parser r a -> Parser r b -> Parser r [a]
p `sepby1` sep = do
  a <- p
  as <- many (sep >> p)
  return (a:as)

spaces :: Parser r ()
spaces = many (sat isSpace) >> return ()

-- | Parse a token with specific parser, throw away any trailing spaces
token :: Parser r a -> Parser r a
token p = spaces >> p

-- | Parse a specified string
string :: String -> Parser r String
string = mapM char

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Line)
           | Paragraph [Line]
           | UnorderedList [Line]
           | BlockQuote [Line]
  deriving (Show,Eq)

-- |Represents line as list of inline elements (words)
data Line = Empty | NonEmpty [Inline]
  deriving (Show,Eq)

-- |Represent inline entity, just a string for this moment
-- Что делать с пунктуацией и пробелами?
data Inline = Plain String
            | Bold String
            | Italic String
            | Monospace String
  deriving (Show,Eq)

-----------------------------------------------------------------
-------------------Parsers for Inline elements-------------------
-----------------------------------------------------------------

-------------------Helper Parsers-----------------
punctuation :: Parser r Char
punctuation = foldl1 alt (map char marks)
  where marks = ".,:;!?'-"

alphanumsWithPunctuation :: Parser r String
alphanumsWithPunctuation = some (alphanum `alt` punctuation)

-- |Sequence of alphanums with punctuations and spaces
sentence :: Parser r String
sentence =
  some (do
    w <- alphanumsWithPunctuation
    s <- (many (char ' '))
    return $ w ++ s
  ) >>= pure . concat
