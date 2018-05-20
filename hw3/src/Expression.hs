{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module Expression
    ( Expr (..)
    , ExprError
    , eval
    , ExprMap
    ) where

import           Control.Monad.Catch  (Exception, MonadThrow, throwM)
import           Control.Monad.Reader (MonadReader, asks, liftM2, local,
                                       runReaderT)
import qualified Data.Map             as M

data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Let String Expr Expr
    deriving Eq

instance Show Expr where
    show :: Expr -> String
    show (Lit x)        = show x
    show (Var x)        = show x
    show (Add x y)      = show x ++ " + " ++ show y
    show (Sub x y)      = show x ++ " - " ++ show y
    show (Mul x y)      = show x ++ " * " ++ show y
    show (Div x y)      = show x ++ " / " ++ show y
    show (Let x e1 e2)  = "let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2

data ExprError
    = MissingVariable String
    | DivisionByZero
    deriving Eq

instance Show ExprError where
    show (MissingVariable v) = "Variable " ++ v ++ " missing"
    show DivisionByZero      = "Division by zero"

instance Exception ExprError

type ExprMap = M.Map String Int

evalImpl :: (MonadReader ExprMap m, MonadThrow m) => Expr -> m Int
evalImpl (Lit n) = return n
evalImpl (Var v) = do
    var <- asks (M.lookup v)
    maybe (throwM (MissingVariable v)) return var
evalImpl (Add lhs rhs) = liftM2 (+) (evalImpl lhs) (evalImpl rhs)
evalImpl (Sub lhs rhs) = liftM2 (-) (evalImpl lhs) (evalImpl rhs)
evalImpl (Mul lhs rhs) = liftM2 (*) (evalImpl lhs) (evalImpl rhs)
evalImpl (Div lhs rhs) = do
    r <- evalImpl rhs
    if r == 0
    then throwM DivisionByZero
    else do
        l <- evalImpl lhs
        return $ l `div` r
evalImpl (Let v val expr) = do
    res <- evalImpl val
    local (M.insert v res) (evalImpl expr)

eval :: MonadThrow m => Expr -> ExprMap -> m Int
eval expr = runReaderT $ evalImpl expr
