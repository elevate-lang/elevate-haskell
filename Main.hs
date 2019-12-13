import UntypedLambdaCalculus.Core
import UntypedLambdaCalculus.Strategies
import UntypedLambdaCalculus.Definitions
import Elevate
import Strategies

expr1 = App (Abs "y" (Var "y")) (Var "a")

-- (λx.x) ((λx.x) (λz. (λx.x) z))
idExpr = Abs "x" (Var "x")
expr = App idExpr (App idExpr (Abs "z" (App idExpr (Var "z"))))
strategy = repeat' (function strategy)

simplified = apply callByValue expr

applyAndPrint:: Strategy Expr -> Expr -> String
applyAndPrint s e = generateDerivation e (apply s e)

bools = App (App (App test tru) (Var "v")) (Var "w")

main :: IO ()
main = do
    putStrLn ""
    putStrLn ((show "input: ") ++ (show expr))
    putStrLn ""
    putStrLn (show "NormalOrder:")
    putStrLn (applyAndPrint normalOrder expr)
    putStrLn ""
    putStrLn (show "CallByValue:")
    putStrLn (applyAndPrint callByValue expr)
    putStrLn ""
    putStrLn (show "CallByName:")
    putStrLn (applyAndPrint callByName expr)
    putStrLn ""
    putStrLn ""
    putStrLn (show "Boolean:")
    putStrLn (applyAndPrint normalOrder bools)



