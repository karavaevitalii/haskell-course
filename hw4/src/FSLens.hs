{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSLens where

import           Control.Lens

import qualified System.Directory.Tree as D
import           System.FilePath       (replaceExtension)

data FS
    = Dir
          { _name     :: FilePath
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath
          }
    deriving Show

scan :: FilePath -> IO FS
scan root = mapTree . D.dirTree <$> D.readDirectoryWith (\_ -> return ()) root
  where
    mapTree :: D.DirTree () -> FS
    mapTree (D.File n _)      = File n
    mapTree (D.Dir n content) = Dir n (map mapTree content)
    mapTree (D.Failed _ err)  = error (show err)

makeLenses ''FS
makePrisms ''FS

cd :: String -> Traversal' FS FS
cd path = contents . traversed . filtered (\d -> isn't _File d && _name d == path)

ls :: Traversal' FS FilePath
ls = contents . traversed . name

file :: String -> Traversal' FS String
file fname = contents . traversed . filtered (\d -> isn't _Dir d && _name d == fname) . name

changeExtension :: String -> FS -> FS
changeExtension newExtension =  contents . traversed . filtered isFile . name %~
    (`replaceExtension` newExtension)
  where
    isFile :: FS -> Bool
    isFile (File _) = True
    isFile _        = False

fileNames :: FS -> [FilePath]
fileNames fs = fs ^.. ls ++ concatMap fileNames (fs ^.. contents . traverse)

rmempty :: String -> FS -> FS
rmempty dirname root = root & contents %~
    filter (\fs -> isn't _Dir fs || (_name fs /= dirname) || not (null (fs ^. contents)))
