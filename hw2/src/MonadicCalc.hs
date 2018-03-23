module MonadicCalc
    ( Expr (..)
    , ArithmeticError (..)
    , eval
    , bin
    ) where

import           Control.Monad

data Expr = Const Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving Eq

instance Show Expr where
    show (Const x)     = show x
    show (Add lhs rhs) = show lhs ++ " + " ++ show rhs
    show (Sub lhs rhs) = show lhs ++ " - " ++ show rhs
    show (Mul lhs rhs) = show lhs ++ " * " ++ show rhs
    show (Div lhs rhs) = show lhs ++ " / " ++ show rhs
    show (Pow lhs rhs) = show lhs ++ " ^ " ++ show rhs

data ArithmeticError = DivisionByZero
    | NegativeExponent
    deriving Eq

instance Show ArithmeticError where
    show DivisionByZero = "Division by zero"
    show _              = "Negative exponent"

eval :: Expr -> Either ArithmeticError Int
eval (Const x)      = return x
eval (Add lhs rhs)  = liftM2 (+) (eval lhs) (eval rhs)
eval (Sub lhs rhs)  = liftM2 subtract (eval rhs) (eval lhs)
eval (Mul lhs rhs)  = liftM2 (*) (eval lhs) (eval rhs)
eval (Div lhs rhs)  = case eval rhs of
    (Right 0)   -> Left DivisionByZero
    r@(Right _) -> liftM2 div (eval lhs) r
    r@(Left _)  -> r
eval (Pow lhs rhs)  = case eval rhs of
    r@(Right e) -> if e < 0
                   then Left NegativeExponent
                   else liftM2 (^) (eval lhs) r
    r@(Left _) -> r

bin :: Int -> [[Int]]
bin n
    | n < 0     = error "Illegal argument"
    | n == 0    = [[]]
    | otherwise = bin (n - 1) >>= \subSeq -> [0:subSeq, 1:subSeq]
