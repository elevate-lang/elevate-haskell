{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Elevate where 

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.List

-- Strategy
-- type Strategy p = p -> Rewrite p

-- If i know the type of the strategy I know the type of the program
class Strategy s p | s -> p where
   ($$) :: s -> p -> Rewrite p

data Rewrite p = Success p
               | Failure -- (Strategy p)

-- Rewrite GADT
--data Rewrite p where
--    Success :: p -> Rewrite p
--    Failure :: (Strategy s) p => s -> Rewrite s

-- Trace: [(Redex, Rule, Result)]
-- Trace is stored "backwards": (head t) is the most recent step
type Trace = [RewriteStep]
type RewriteStep = (String, String, String)

-- Naive Strategies
-- id' :: Show p => Strategy p 
-- id' = \p -> Success p
-- 
-- fail' :: Strategy p
-- fail' = \_ -> Failure fail'

data Id' p = Id'
instance Strategy (Id' p) p where
    Id' $$ p = Success p

data Fail' p = Fail'
instance Strategy (Fail' p) p where
    Fail' $$ p = Failure 


-- Trace Utils
trace :: Show p => p -> String -> p -> Trace 
trace redex rule result = [(show redex, rule, show result)]

-- Make everything look nice
instance Show p => Show (Rewrite p) where
    show (Success p) = "Success " ++ show p 
    show Failure     = "Failure" 


-- instances: Functor / Applicative / Monad / Alternative / MonadPlus
instance Functor Rewrite where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Success p) = Success (f p)
    fmap _ Failure     = Failure  -- we just give up

-- are we acutally using this?
instance Applicative Rewrite where
    pure = \p -> Success p 
    (Success f) <*> m = f <$> m
    Failure     <*> m = Failure 

instance Monad Rewrite where
    return = pure
    (Success p) >>= b = b p -- former 'flatMapSuccess'
    Failure     >>= b = Failure 

instance Alternative Rewrite where
    empty = Failure 
    (Success p) <|> _ = Success p -- former 'flatMapFailure'
    Failure     <|> m = m

instance MonadPlus Rewrite where
    mzero = Failure 
    mplus = (<|>)


-- UTILS 

-- print trace
printRewriteStep :: RewriteStep -> String
printRewriteStep (r,s,p) = r ++ " " ++ s ++ " " ++ p

printTrace :: Trace -> String
printTrace [x] = printRewriteStep x
printTrace (x:xs)    = printTrace xs ++ "\n" ++ printRewriteStep x
