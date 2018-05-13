module Statement
    ( Statement (..)
    , StatementError (..)
    ) where

import           Expression          (Expr (..))

import           Control.Monad.Catch (Exception)

data Statement
    = Def String Expr
    | Assignement String Expr
    | Print Expr
    | Read String
    deriving Show

data StatementError
    = UndefinedVariable String
    | Redefinition String
    deriving Eq

instance Show StatementError where
    show (UndefinedVariable v) = "Variable " ++ v ++ " was not defined"
    show (Redefinition v)      = "Variable " ++ v ++ " already defined"

instance Exception StatementError
