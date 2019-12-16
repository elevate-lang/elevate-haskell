module ElevateMaybe where 

import Control.Applicative
import Control.Monad

-- Strategy
type Transformation p = p -> RewriteResult p 
data Strategy p = Strategy (Transformation p) String

apply :: Strategy p -> p -> RewriteResult p
apply (Strategy s n) p = s p
($$) = apply

-- Make everything look nice
instance Show p => Show (RewriteResult p) where
    show (Success p) = "Success " ++ show p
    show Failure     = "Failure"

instance Show p => Show (Strategy p) where
    show (Strategy t n) = show n

-- RewriteResult (essentially it's a Maybe)
data RewriteResult p = Success p 
                     | Failure 

-- Functor / Applicative / Monad / Alternative / MonadPlus
instance Functor RewriteResult where
    fmap f (Success p) = Success (f p) -- former 'mapSuccess'
    fmap _ Failure     = Failure

instance Applicative RewriteResult where
    pure = Success
    (Success f) <*> m = f <$> m
    Failure     <*> _ = Failure

instance Monad RewriteResult where
    return = Success
    (Success p) >>= b = (b p) -- former 'flatMapSuccess'
    Failure     >>= b = Failure

instance Alternative RewriteResult where
    empty = Failure
    (Success p) <|> _ = Success p -- former 'flatMapFailure'
    Failure     <|> m = m

instance MonadPlus RewriteResult where
    mzero = Failure
    mplus = (<|>)
