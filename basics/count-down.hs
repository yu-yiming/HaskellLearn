{-
    Count down is a program that output all possible combination of given numbers
    zipped with any operators that result in a target.

    Due to commutivity of some operators, all except one among some combinations
    will be abandoned. For example 1+2 and 2+1 are basically the same, so only one
    is left.
-}

-- Operators
data Op = Add | Sub | Mul | Div | Pow

ops :: [Op]
ops = [Add, Sub, Mul, Div]

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"

-- check validity of an arrangement
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Pow _ _ = True

-- apply the operator 
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val x) = show x
    show (App op l r) = show' l ++ show op ++ show' r
        where
            show' (Val x) = show x
            show' e = "(" ++ show e ++ ")"

-- build integer list from an expression.
values :: Expr -> [Int]
values (Val x) = [x]
values (App _ l r) = values l ++ values r

-- evaluate the given expression and get the result put in a list.
-- e.g. App Add (Val 1) (App Mul (Val 2) (Val 3)) will output [7]
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op l r) = [apply op x y | x <- eval l, y <- eval r, valid op x y]

-- get all subsequences of a list.
subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x : xs) = yss ++ map (x:) yss
    where yss = subseqs xs

-- get all arrangement of inserting an element to a list
-- e.g. interleaving 42 with [1, 2, 3] is [[42, 1, 2, 3], [1, 42, 2, 3], [1, 2, 42, 3], [1, 2, 3, 42]]
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y:) (interleave x ys)

-- get all permutations of a list of elements.
perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

-- get the power set of a list of elements.
powerset :: [a] -> [[a]]
powerset = concatMap perms . subseqs

splits :: [a] -> [([a], [a])]
splits [] = []
splits [_] = []
splits (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- splits xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [x] = [Val x]
exprs xs = [e | (ls, rs) <- splits xs, l <- exprs ls, r <- exprs rs, e <- [App op l r | op <- ops]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [x] = [(Val x, x) | x > 0]
results xs = [res | (ls, rs) <- splits xs, l <- results ls, r <- results rs, res <- combine l r]
    where
        combine :: Result -> Result -> [Result]
        combine (l, x) (r, y) = [(App op l r, apply op x y) | op <- ops, valid op x y]

solutions :: [Int] -> Int -> [Expr]
solutions xs x = [e | xs' <- powerset xs, (e, y) <- results xs', x == y]

main :: IO ()
main = print (solutions [1, 3, 7, 10, 25, 50] 765)