module Elevate where 

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Either
import Util

-- Strategy
type Strategy p = p -> Rewrite p

-- Make everything look nice
instance Show p => Show (Rewrite p) where
    show (Success p t) = "Success " ++ show p ++ "\nTrace:\n" ++ printTrace t
    show (Failure s)   = "Failure" 

-- Naive Strategies
id' :: Show p => Strategy p 
id' = \p -> Success p (addTrace p "id'" p)

fail' :: Strategy p
fail' = \_ -> Failure fail'

addTrace :: Show p => p -> String -> p -> Trace 
addTrace redex rule result = [show result] ++ [show redex ++ " [" ++ rule ++ "]" ] 

-- Rewrite Monad
data Rewrite p = Success p Trace
               | Failure (Strategy p)
type Trace = [String]

appendTrace :: Trace -> Rewrite b -> Rewrite b
appendTrace t1 (Success p t2) = Success p (t2 ++ t1)
appendTrace _  (Failure s)   = Failure s

-- instances: Functor / Applicative / Monad / Alternative / MonadPlus
instance Functor Rewrite where
    fmap f (Success p t) = Success (f p) t
    fmap _ (Failure _)   = Failure fail' -- we just give up

-- I would like to have this in fmap as well but there p might not implement show
updateTrace :: Show p => Rewrite p -> Rewrite p
updateTrace (Success p (t:ts)) = Success p ((show p):ts)
updateTrace (Failure s)        = Failure s
(<$$>) :: Show b => (a -> b) -> Rewrite a -> Rewrite b
(<$$>) f x = updateTrace (f <$> x)

-- are we acutally using this?
instance Applicative Rewrite where
    pure = \p -> Success p [""]
    (Success f t) <*> m = f <$> m
    (Failure s)   <*> m = Failure fail'

instance Monad Rewrite where
    return = pure
    (Success p t) >>= b = appendTrace t (b p) -- former 'flatMapSuccess'
    (Failure s)   >>= b = Failure fail'

instance Alternative Rewrite where
    empty = Failure fail'
    (Success p t) <|> _ = Success p t -- former 'flatMapFailure'
    (Failure s)   <|> m = m

instance MonadPlus Rewrite where
    mzero = Failure fail'
    mplus = (<|>)
