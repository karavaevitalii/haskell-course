{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Walker where

import           FSLens

import           Control.Lens
import           Control.Monad.State (MonadIO, MonadState, get, liftIO, modify,
                                      runStateT)
import           Data.List           (intercalate)

data Walker = Walker
  { _root    :: FilePath
  , _visited :: [FS]
  , _fileCnt :: Int
  , _dirCnt  :: Int
  }

makeLenses ''Walker

instance Show Walker where
    show w =
        "You in " ++
        (\(Walker _ v _ _) -> intercalate "/" $ reverse $ map (^. name) v) w ++ "\n" ++
        "Files from root " ++ show (w ^. root) ++ " " ++ show (w ^. fileCnt) ++ "\n" ++
        "Directories from root " ++ show (w ^. root) ++ " " ++ show (w ^. dirCnt)

data Command
    = Cd FilePath
    | Up

parseCommand :: String -> Maybe Command
parseCommand cmd = let splitted = words cmd in
    case length splitted of
        1 -> case head splitted of
            "up" -> Just Up
            _    -> Nothing
        2 -> case head splitted of
            "cd" -> Just $ Cd (last splitted)
            _    -> Nothing
        _ -> Nothing

walker :: FilePath -> IO()
walker path = do
    root <- scan path

    let dirCnt  = length (root ^.. contents . traversed . filtered (isn't _File))
    let fileCnt = length (root ^.. contents . traversed . filtered (isn't _Dir))
    let state = Walker (root ^. name) [root] dirCnt fileCnt

    _ <- runStateT impl state

    return ()
  where
    impl :: (MonadState Walker m, MonadIO m) => m ()
    impl = do
        env <- get

        liftIO $ print env
        liftIO $ putStr "> "

        cmd <- liftIO getLine
        case parseCommand cmd of
            Just (Cd path) -> case head (env ^. visited) ^? cd path of
                Just dir -> do
                    let dCnt = length (dir ^.. contents . traversed . filtered (isn't _File))
                    let fCnt = length (dir ^.. contents . traversed . filtered (isn't _Dir))

                    modify $ (%~) visited (dir :)
                    modify $ (%~) dirCnt (+ dCnt)
                    modify $ (%~) fileCnt (+ fCnt)

                Nothing -> liftIO $ putStrLn $ "no such directory: " ++ path

            Just Up -> if (env ^. root) == (head (env ^. visited) ^. name)
                       then liftIO $ putStrLn "can't go up from root directory"
                       else do
                            let dCnt = length (head (env ^. visited) ^.. contents . traversed . filtered (isn't _File))
                            let fCnt = length (head (env ^. visited) ^.. contents . traversed . filtered (isn't _Dir))

                            modify $ (%~) visited tail
                            modify $ (%~) dirCnt $ subtract dCnt
                            modify $ (%~) fileCnt $ subtract fCnt

            Nothing -> liftIO $ putStrLn "unknown command"

        impl
