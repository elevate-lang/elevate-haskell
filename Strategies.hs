module Strategies where 

import ElevateMaybe
import Control.Applicative

-- Naive Strategies
id' :: Strategy p 
id' = Strategy (\p -> Success p) "id'"

fail' :: Strategy p
fail' = Strategy (\p -> Failure) "fail'"

-- Basic Combinators
seq' :: Strategy p -> Strategy p -> Strategy p
seq' f s = Strategy (\p -> (f $$ p) >>= (\p -> s $$ p)) "seq'"
(~>>) = seq' 

lChoice' :: Strategy p -> Strategy p -> Strategy p
lChoice' f s = Strategy (\p -> (f $$ p) <|> (s $$ p)) "lChoice'"
(<+) = lChoice'

-- Basic Strategies
try' :: Strategy p -> Strategy p
try' s = s <+ id'

repeat' :: Strategy p -> Strategy p
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
normalize :: Traversable' p => Strategy p -> Strategy p
normalize s = repeat' . oncetd $ s
