{-# LANGUAGE TemplateHaskell #-}

module TH
    ( chooseByIndices
    , showTextFunc
    ) where

import           Control.Monad       (replicateM)
import           Data.Text           (Text, pack)
import           Language.Haskell.TH (Dec (..), Exp (..), Name, Q, conT, lamE,
                                      newName, tupE, tupP, varE, varP)

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n ids = do
    as <- replicateM n (newName "a")
    lamE [tupP (map varP as)] $ tupE (map (map varE as !!) ids)

class ShowText a where
    showText :: a -> Text

showTextFunc :: Name -> Q [Dec]
showTextFunc name = [d|instance ShowText $(conT name) where showText = pack . show|]
