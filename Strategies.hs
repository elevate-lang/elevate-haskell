module Strategies where 

import Elevate
import Control.Applicative

-- Basic Combinators
seq' :: Strategy p -> Strategy p -> Strategy p
seq' f s = \p -> (f p) >>= (\p -> s p)
(~>>) = seq' 

lChoice' :: Strategy p -> Strategy p -> Strategy p
lChoice' f s = \p -> (f p) <|> (s p)
(<+) = lChoice'

-- Basic Strategies
try' :: Show p => Strategy p -> Strategy p
try' s = s <+ id'

repeat' :: Show p => Strategy p -> Strategy p
repeat' s = try' (s ~>> (repeat' s))

-- Traversable
class Traversable' p where
    all' :: Strategy p -> Strategy p 
    one' :: Strategy p -> Strategy p 
    -- some' :: Strategy p -> Strategy p 

-- Complete Traversals
oncetd :: Traversable' p => Strategy p -> Strategy p
oncetd s = s <+ (one' . oncetd $ s)

oncebu :: Traversable' p => Strategy p -> Strategy p
oncebu s = (one' . oncebu $ s) <+ s

topdown :: Traversable' p => Strategy p -> Strategy p
topdown s = s ~>> (all' . topdown $ s)

-- Normalize
normalize :: Show p => Traversable' p => Strategy p -> Strategy p
normalize s = repeat' . oncetd $ s

-- Predicates
toBool :: Rewrite p -> Bool
toBool (Success p t) = True
toBool (Failure s)   = False

not' :: Show p => Strategy p -> Strategy p
not' s = \a -> case (s a) of 
    (Success x t) -> Failure (not' s)
    (Failure x)  -> Success a (trace a "not" a)

is' :: (Eq p, Show p) => p -> Strategy p
is' a = \p -> if (a == p) then (Success p (trace p "is" p)) else Failure (is' p)

contains' :: (Traversable' p, Eq p, Show p) => p -> Strategy p 
contains' a = oncetd (is' a)
