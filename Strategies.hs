module Strategies where 

import Elevate

-- Naive Strategies
id' :: Strategy p 
id' = Strategy (\p -> success p id' p) "id'"

fail' :: Strategy p
fail' = Strategy (\p -> Failure fail') "fail'"

-- Basic Combinators
seq' :: Strategy p -> Strategy p -> Strategy p
seq' f s = Strategy (\p -> flatMapSuccess s (apply f p)) "seq'"

lChoice' :: Strategy p -> Strategy p -> Strategy p
lChoice' f s = Strategy (\p -> flatMapFailure (\_ -> (apply s p)) (apply f p)) "lChoice'"

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
