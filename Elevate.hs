module Elevate where 

import Data.List
import Data.Maybe

-- Strategy
type Transformation p = p -> RewriteResult p 
data Strategy p = Strategy (Transformation p) String

apply :: Strategy p -> p -> RewriteResult p
apply (Strategy s n) p = s p

($$) :: Strategy p -> p -> RewriteResult p 
($$) = apply

-- Trace: [(Redex, Rule, Result)] 
data RewriteStep p = RewriteStep p (Strategy p) p
type Trace p = [RewriteStep p]

-- RewriteResult
data RewriteResult p = Success p (Trace p)
                     | Failure (Strategy p) 

--instance Functor Strategy where
--    fmap f (Strategy t n) = Strategy (\a -> (f <$> (t a))) n
--
--instance Functor RewriteStep where
--    fmap f (RewriteStep r s p) = RewriteStep (f r) (f <$> s) (f p)
--
--instance Functor RewriteResult where
--    -- or only modify the head?
--    fmap f (Success p t) = Success (f p) (map (fmap f) t)

-- Utils
success :: p -> Strategy p -> p -> RewriteResult p
success r s p = Success p [RewriteStep r s p]

appendTrace :: Trace p -> RewriteResult p -> Strategy p -> RewriteResult p
appendTrace t1 (Success p t2) s = Success p (t1 ++ t2)
appendTrace t (Failure x) s     = Failure x

-- Make everything look nice
instance Show p => Show (RewriteResult p) where
    show (Success p t) = "Success " ++ show p ++ "\nTrace:\n" ++ show t
    show (Failure _)   = "Failure"

instance Show p => Show (RewriteStep p) where
    show (RewriteStep r s p) = 
        "\nredex  : " ++ show r ++ 
        "\nrule   : " ++ show s ++ 
        "\nresult : " ++ show p ++ "\n"

instance Show p => Show (Strategy p) where
    show (Strategy t n) = show n

-- RewriteResult Utils
mapSuccess :: (p -> p) -> RewriteResult p -> RewriteResult p
mapSuccess f (Success p ((RewriteStep r s e):ts)) = Success (f p) (RewriteStep r s (f e):ts)
mapSuccess f (Failure s)   = Failure s

flatMapSuccess :: Strategy p -> RewriteResult p -> RewriteResult p
flatMapSuccess f (Success p t) = appendTrace t (f $$ p) f
flatMapSuccess f (Failure s)   = Failure s

mapFailure :: (Strategy p -> Strategy p) -> RewriteResult p -> RewriteResult p
mapFailure f (Success p t) = Success p t
mapFailure f (Failure s)   = Failure (f s)

flatMapFailure :: (Strategy p -> RewriteResult p) -> RewriteResult p -> RewriteResult p
flatMapFailure f (Success p t) = Success p t
flatMapFailure f (Failure s)   = f s

-- More Util Functions
isSuccess :: RewriteResult p -> Bool
isSuccess (Success p t) = True
isSuccess (Failure _)   = False

get :: RewriteResult p -> Maybe p
get (Success p t) = Just p
get (Failure _)   = Nothing

-- Pretty printing the derivation
generateDerivation :: Show p => p -> RewriteResult p -> String
generateDerivation _ (Failure _)   = "Failure"
generateDerivation p (Success _ t) = 
    (fst (foldl (foldOp (\x -> whitespaceToRule x (maxExprLength p t))) ("", p) t)) ++ (show (case (last t) of
        RewriteStep r s p -> p))

maxExprLength :: Show p => p -> Trace p  -> Int
maxExprLength p t = foldl (\acc x -> case x of
    RewriteStep r s e -> max acc (length (show e))
        ) (length (show p)) t

whitespaceToRule :: Show p => p -> Int -> String
whitespaceToRule p l = replicate (l - (length (show p)) + 2) ' '

foldOp :: Show p => (p -> String) -> (String, p) -> RewriteStep p -> (String, p)
foldOp wsToRule (acc, parent) (RewriteStep redex rule result) = (
    acc ++ (show parent) ++ (wsToRule parent) ++ "[" ++ (show rule) ++ "]\n" ++
    (underline (fromJust (findString (show redex) (show parent))) (length (show redex))) ++ "\n",
    result)

underline :: Int -> Int -> String
underline ws dash = replicate ws ' ' ++ replicate dash '-'

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)
