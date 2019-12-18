module Util where

-- print trace
printTrace :: [String] -> String
printTrace [x]    = x
printTrace (x:xs) = printTrace xs ++ "\n" ++ x


