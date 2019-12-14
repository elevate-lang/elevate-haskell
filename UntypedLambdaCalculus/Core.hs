module UntypedLambdaCalculus.Core where 

-- Untyped Lambda Calculus
type Name = String 
data Expr = Var Name 
          | Abs Name Expr 
          | App Expr Expr 
          deriving (Eq)

instance Show Expr where
    -- defs
    show (Abs x1 (Var x2)) | x1 == x2 = "id"
    show (Abs t1 (Abs f (Var t2))) | t1 == t2 = "tru"
    show (Abs t (Abs f1 (Var f2))) | f1 == f2 = "fls"
    -- core
    show (Var x) = x
    show (Abs x y) = "λ" ++ x ++ "." ++ show y
    show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"

--            what -> with -> in 
substitute :: Name -> Expr -> Expr -> Expr
substitute a b (Var x)   | a == x = b
substitute a b (Abs x y) | a /= x = Abs x (substitute a b y)
substitute a b (App x y)          = App (substitute a b x) (substitute a b y)
substitute _ _ i = i
