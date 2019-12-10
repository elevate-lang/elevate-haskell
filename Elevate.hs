module Elevate where 

-- Strategy
type Strategy p = p -> RewriteResult p

-- RewriteResult
data RewriteResult p = Success p 
                     | Failure (Strategy p) 

instance Show p => Show (RewriteResult p) where
    show (Success p) = "Success " ++ show p
    show (Failure _) = "Failure"

mapSuccess :: (p -> p) -> RewriteResult p -> RewriteResult p
mapSuccess f (Success p) = Success (f p)
mapSuccess f (Failure s) = Failure s

flatMapSuccess :: (p -> RewriteResult p) -> RewriteResult p -> RewriteResult p
flatMapSuccess f (Success p) = f p
flatMapSuccess f (Failure s) = Failure s

mapFailure :: (Strategy p -> Strategy p) -> RewriteResult p -> RewriteResult p
mapFailure f (Success p) = Success p
mapFailure f (Failure s) = Failure (f s)

flatMapFailure :: (Strategy p -> RewriteResult p) -> RewriteResult p -> RewriteResult p
flatMapFailure f (Success p) = Success p
flatMapFailure f (Failure s) = f s

-- Util Functions
isSuccess :: RewriteResult p -> Bool
isSuccess (Success p) = True
isSuccess (Failure _) = False

get :: RewriteResult p -> Maybe p
get (Success p) = Just p
get (Failure _) = Nothing
