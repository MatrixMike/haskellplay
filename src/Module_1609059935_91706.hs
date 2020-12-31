module Module_1609059935_91706 where

--------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Yoctoparsec
-- Copyright   : (C) 2016 mniip
-- License     : MIT
-- Maintainer  : mniip <mniip@mniip.com>
-- Stability   : experimental
-- Portability : portable
--------------------------------------------------------------------------------

import Data.List (uncons)
import Control.Applicative ((<|>),  Alternative(empty) )
import Control.Monad ( MonadPlus )
import Control.Monad.State ( StateT(StateT, runStateT) )
import Control.Monad.Trans.Free ( FreeT(FreeT), iterTM, FreeF(Pure, Free) )
import Control.Monad (mfilter)

-- | A @Parser b t a@ is a parser that consumes a stream of @t@ tokens and as a
-- result yields a value of type @a@, while operating under the @b@
-- non-determinism monad. For most purposes @b@ should be a 'MonadPlus'. Useful
-- examples include @[]@ if you want backtracking, 'Maybe' if you want no
-- backtracking, @'StateT' []@ if you want to maintain a state that is
-- automatically reverted when backtracking, and so on. 'hoistFreeT' can be used
-- to change the backtracking monad.
--
-- 'FreeT' provides us with instances for 'Functor', 'Applicative', 'Monad',
-- 'Alternative' and 'MonadPlus'.
type Parser b t a = FreeT ((->) t) b a

-- | A trivial parser that consumes a single token and yields it. Other parsers
-- can be derived from this one using methods of the aforementioned typeclasses.
-- For example,
-- @
-- char x = mfilter (== x) token
-- @
token :: Applicative b => Parser b t t
token = FreeT . pure . Free $ FreeT . pure . Pure

-- | Apply a parser to a stream given a function that obtains the next character
-- from the stream within the same non-determinism monad.
parseStream :: Monad b => (s -> b (t, s)) -> Parser b t a -> s -> b (a, s)
parseStream next = runStateT . iterTM (StateT next >>=)

-- | Parse a string. When the end of the string is encountered, 'empty' is
-- yielded into the non-determinism monad.
parseString :: MonadPlus b => Parser b t a -> [t] -> b (a, [t])
parseString = parseStream (maybe empty pure . uncons)

char :: (MonadPlus b, Eq a) => a -> FreeT ((->) a) b a
char c = mfilter (==c) token

string :: (Traversable t, MonadPlus b1, Eq b2) => t b2 -> FreeT ((->) b2) b1 (t b2)
string = mapM char

choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = foldl (<|>) empty

p :: FreeT ((->) Char) [] [Char]
p = choice
    [ fmap return (char 'h')
    , string "he"
    , string "hel"
    , string "hell"
    ]

p2 :: FreeT ((->) Char) [] [Char]
p2 = (string "he"  *> string "llo")
 <|> (string "hel" *> string "lo")

-- >>> parseString p "hello"
-- [("h","ello"),("he","llo"),("hel","lo"),("hell","o")]

-- >>> parseString p2 "hello"
-- [("llo",""),("lo","")]



