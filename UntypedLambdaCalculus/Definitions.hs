module UntypedLambdaCalculus.Definitions where

import UntypedLambdaCalculus.Core

-- Church Booleans
tru = Abs "t" (Abs "f" (Var "t"))
fls = Abs "t" (Abs "f" (Var "f"))

test = Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n"))))
and' = Abs "b" (Abs "c" (App (App (Var "b") (Var "c")) fls))

pair = Abs "f" (Abs "s" (Abs "b" (App (App (Var "b") (Var "f")) (Var "s"))))
fst' = Abs "p" (App (Var "p") tru)
snd' = Abs "p" (App (Var "p") fls)
