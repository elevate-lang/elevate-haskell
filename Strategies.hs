module Strategies where 

import Elevate

-- Naive Strategies
id' :: Strategy p
id' p = success p 

fail' :: Strategy p
fail' p = Failure fail'

-- Basic Combinators
seq' :: Strategy p -> Strategy p -> Strategy p
seq' f s p = flatMapSuccess s (f p)

lChoice' :: Strategy p -> Strategy p -> Strategy p
lChoice' f s p = flatMapFailure (\_ -> (s p)) (f p)

-- Basic Strategies
try' :: Strategy p -> Strategy p
try' s = lChoice' s id'

repeat' :: Strategy p -> Strategy p
repeat' s = try' (seq' s (repeat' s))

-- Traversable
class Traversable' p where
    all' :: Strategy p -> Strategy p 
    one' :: Strategy p -> Strategy p 
    -- some' :: Strategy p -> Strategy p 

-- Complete Traversals
oncetd :: Traversable' p => Strategy p -> Strategy p
oncetd s = lChoice' s (one' (oncetd s))

oncebu :: Traversable' p => Strategy p -> Strategy p
oncebu s = lChoice' (one' (oncebu s)) s

topdown :: Traversable' p => Strategy p -> Strategy p
topdown s = seq' s (all' (topdown s))


normalize :: Traversable' p => Strategy p -> Strategy p
normalize s = repeat' (oncetd s)

-- ...
