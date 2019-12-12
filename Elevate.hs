module Elevate where 

-- Strategy
type Transformation p = p -> RewriteResult p 
data Strategy p = Strategy (Transformation p) String

apply :: Strategy p -> p -> RewriteResult p
apply (Strategy s n) p = s p

-- Trace: should be [(Redex, Rule, Result)] eventually
data RewriteStep p = RewriteStep (Strategy p) p
type Trace p = [RewriteStep p]

-- RewriteResult
data RewriteResult p = Success p (Trace p)
                     | Failure (Strategy p) 

-- Utils
success :: Strategy p -> p -> RewriteResult p
success s p = Success p [RewriteStep s p]

appendTrace :: Trace p -> RewriteResult p -> Strategy p -> RewriteResult p
appendTrace t1 (Success p t2) s = Success p (t1 ++ t2)
appendTrace t (Failure x) s     = Failure x

instance Show p => Show (RewriteResult p) where
    show (Success p t) = "Success " ++ show p ++ "\nTrace:\n" ++ show t
    show (Failure _)   = "Failure"

instance Show p => Show (RewriteStep p) where
    show (RewriteStep s p) = show s ++ "\n" ++ show p ++ "\n"

instance Show p => Show (Strategy p) where
    show (Strategy t n) = show n

-- RewriteResult Utils
mapSuccess :: (p -> p) -> RewriteResult p -> RewriteResult p
mapSuccess f (Success p ((RewriteStep s e):ts)) = Success (f p) (RewriteStep s (f e):ts)
mapSuccess f (Failure s)   = Failure s

flatMapSuccess :: Strategy p -> RewriteResult p -> RewriteResult p
flatMapSuccess f (Success p t) = appendTrace t (apply f p) f
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
