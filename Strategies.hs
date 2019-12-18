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
