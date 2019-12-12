import UntypedLambdaCalculus.Core
import UntypedLambdaCalculus.Strategies
import Elevate
import Strategies

expr1 = App (Abs "y" (Var "y")) (Var "a")

-- (λx.x) ((λx.x) (λz. (λx.x) z))
idExpr = Abs "x" (Var "x")
expr = App idExpr (App idExpr (Abs "z" (App idExpr (Var "z"))))
strategy = repeat' (function strategy)

simplified = apply callByValue expr

applyAndPrint:: Strategy Expr -> String
applyAndPrint s = generateDerivation expr (apply s expr)

main :: IO ()
main = do
    putStrLn ""
    putStrLn ((show "input: ") ++ (show expr))
    putStrLn ""
    putStrLn (show "NormalOrder:")
    putStrLn (applyAndPrint normalOrder)
    putStrLn ""
    putStrLn (show "CallByValue:")
    putStrLn (applyAndPrint callByValue)
    putStrLn ""
    putStrLn (show "CallByName:")
    putStrLn (applyAndPrint callByName)


