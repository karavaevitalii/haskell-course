{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSLens where

import           Control.Lens

import           System.Directory.Tree (AnchoredDirTree (dirTree))
import qualified System.Directory.Tree
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

--scan :: FilePath -> IO FS
--scan root = mapTree . dirTree <$> readDirectoryWith (\_ -> return ()) root
--  where
--    mapTree :: DirTree () -> FS
--    mapTree (File n _)      = File n
--    mapTree (Dir n content) = Dir n (map mapTree content)
--    mapTree (Failed _ err)  = error (show err)

makeLenses ''FS
makePrisms ''FS

cd :: String -> Traversal' FS FS
cd path = contents.traversed.filtered(\d -> isn't _File d && (_name d) == path)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

file :: String -> Traversal' FS String
file fname = contents.traversed.filtered(\d -> isn't _Dir d && (_name d) == fname).name

changeExtension :: String -> FS -> FS
changeExtension newExtension =  contents . traversed . filtered isFile . name %~ 
    (`replaceExtension` newExtension)
  where 
    isFile :: FS -> Bool
    isFile (File _) = True
    isFile _        = False

rmempty :: String -> FS -> FS
rmempty dirname root = root & contents %~
    filter (\fs -> isn't _Dir fs || (_name fs /= dirname) || (not $ null (fs^.contents)))
