{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Program
    ( runProgram
    ) where

import           Expression          (ExprMap, eval)
import           Parser              (parseProgram)
import           Statement           (Statement (..), StatementError (..))

import           Control.Monad       (replicateM_)
import           Control.Monad.Catch (MonadCatch, throwM)
import           Control.Monad.State (MonadIO, MonadState, forM_, get, liftIO,
                                      modify, runStateT)
import qualified Data.Map            as M
import           Data.Maybe          (isNothing)

defS :: (MonadState ExprMap m, MonadCatch m) => String -> Int -> m ()
defS var val = do
    vars <- get
    if isNothing $ M.lookup var vars
    then modify $ M.insert var val
    else throwM $ Redefinition var

assignS :: (MonadState ExprMap m, MonadCatch m) => String -> Int -> m ()
assignS var val = do
    vars <- get
    if isNothing $ M.lookup var vars
    then throwM $ UndefinedVariable var
    else modify $ M.insert var val

readS :: (MonadState ExprMap m, MonadCatch m) => String -> Int -> m ()
readS var val = modify $ M.insert var val

printS :: (MonadIO m) => Int -> m ()
printS val = liftIO $ print val

execute :: (MonadState ExprMap m, MonadCatch m, MonadIO m) => [Statement] -> m ()
execute stmts = forM_ stmts $ \stmt -> do
    vars <- get
    val  <- case stmt of
        (Def _ e)         -> eval e vars
        (Assignement _ e) -> eval e vars
        (Read _)          -> read <$> liftIO getLine
        (Print e)         -> eval e vars
        (Loop e _ _)      -> eval e vars
    case stmt of
        Def var _         -> defS var val
        Assignement var _ -> assignS var val
        Read var          -> readS var val
        Print _           -> printS val
        Loop _ var body   -> do
            to <- eval var vars
            replicateM_ (to - val) (execute body)

runStmts :: [Statement] -> IO ()
runStmts stmts = do
    let output = execute stmts
    fst <$> runStateT output M.empty

runProgram :: String -> String -> IO ()
runProgram name program = do
    stmts <- parseProgram name program
    runStmts stmts
