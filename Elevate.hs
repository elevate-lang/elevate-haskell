module Elevate where 

-- Strategy
type Strategy p = p -> RewriteResult p

-- Trace
type Trace p = [p]

-- RewriteResult
data RewriteResult p = Success p (Trace p)
                     | Failure (Strategy p) 

-- Utils
success :: p -> RewriteResult p
success p = Success p [p]

appendTrace :: RewriteResult p -> p -> RewriteResult p
appendTrace (Success p1 t) p2 = Success p1 ([p2] ++ t)
appendTrace (Failure x) p     = Failure x

instance Show p => Show (RewriteResult p) where
    show (Success p t) = "Success " ++ show p ++ "\nTrace:\n" ++ show t
    show (Failure _)   = "Failure"

-- RewriteResult Utils
mapSuccess :: (p -> p) -> RewriteResult p -> RewriteResult p
mapSuccess f (Success p t) = Success (f p) t
mapSuccess f (Failure s)   = Failure s

flatMapSuccess :: Strategy p -> RewriteResult p -> RewriteResult p
flatMapSuccess f (Success p t) = appendTrace (f p) p
flatMapSuccess f (Failure s)   = Failure s

mapFailure :: (Strategy p -> Strategy p) -> RewriteResult p -> RewriteResult p
mapFailure f (Success p t) = Success p t
mapFailure f (Failure s)   = Failure (f s)

flatMapFailure :: (Strategy p -> RewriteResult p) -> RewriteResult p -> RewriteResult p
flatMapFailure f (Success p t) = Success p t
flatMapFailure f (Failure s)   = f s

-- Util Functions
isSuccess :: RewriteResult p -> Bool
isSuccess (Success p t) = True
isSuccess (Failure _)   = False

get :: RewriteResult p -> Maybe p
get (Success p t) = Just p
get (Failure _)   = Nothing
