module Elevate where 

import Control.Applicative
import Control.Monad

-- Strategy
type Transformation p = p -> RewriteResult p 
data Strategy p = Strategy (Transformation p) String

apply :: Strategy p -> p -> RewriteResult p
apply (Strategy s n) p = s p
($$) = apply

-- Naive Strategies
id' :: Strategy p 
id' = Strategy (\p -> Success p) "id'"

fail' :: Strategy p
fail' = Strategy (\p -> Failure fail') "fail'"

-- Make everything look nice
instance Show p => Show (RewriteResult p) where
    show (Success p) = "Success " ++ show p
    show (Failure s) = "Failure" ++ show s

instance Show p => Show (Strategy p) where
    show (Strategy t n) = show n

-- RewriteResult 
data RewriteResult p = Success p 
                     | Failure (Strategy p)

-- Functor / Applicative / Monad / Alternative / MonadPlus
instance Functor RewriteResult where
    fmap f (Success p) = Success (f p) -- former 'mapSuccess'
    -- we give up: we failed once, now we double-fail
    fmap f (Failure (Strategy s n)) = Failure fail'

instance Applicative RewriteResult where
    pure = Success
    (Success f) <*> m = f <$> m
    (Failure s) <*> m = Failure fail'

instance Monad RewriteResult where
    return = Success
    (Success p) >>= b = (b p) -- former 'flatMapSuccess'
    (Failure s) >>= b = Failure fail'

instance Alternative RewriteResult where
    empty = Failure fail'
    (Success p) <|> _ = Success p -- former 'flatMapFailure'
    (Failure s) <|> m = m

instance MonadPlus RewriteResult where
    mzero = Failure fail'
    mplus = (<|>)
