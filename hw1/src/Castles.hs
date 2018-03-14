module Castles
  ( Person (..)
  , Club (..)
  , Citizens (..)
  , House (..)
  , Castle (..)
  , Walls (..)
  , Protection (..)
  , City (..)
  , buildCastle
  , buildClub
  , buildHouse
  , getNewLord
  , buildWalls
  ) where

import           Data.List.NonEmpty (NonEmpty (..), (<|))

newtype Person = Person
  { personName :: String }
    deriving (Eq, Show)

data Club = Library
  | Church
    deriving (Show)

data Citizens =
    One
  | Two
  | Three
  | Four
    deriving (Show, Enum)

newtype House = House Citizens deriving (Show)

newtype Castle = Castle
  { castleLord :: Maybe Person }
    deriving (Show)

data Walls = Walls deriving (Show)

data Protection = Protection
  { protectionCastle :: Castle
  , protectionWalls  :: Maybe Walls
  } deriving (Show)

data City = City
  { cityProtection :: Maybe Protection
  , cityClub       :: Maybe Club
  , cityHouses     :: NonEmpty House
  } deriving (Show)

data NewLordError =
    NoCastle
  | LordExists
    deriving (Show)

buildCastle :: City -> Castle -> Either String City
buildCastle (City Nothing club houses) castle =
  Right (City (Just (Protection castle Nothing)) club houses)
buildCastle _ _ = Left "Can not build another one castle"

buildClub :: City -> Club -> Either String City
buildClub (City prot Nothing houses) club = Right (City prot (Just club) houses)
buildClub _ _ = Left "Can not build church or library"

buildHouse :: City -> Citizens -> City
buildHouse (City prot club houses) family = City prot club (House family<|houses)

getNewLord :: City -> Person -> Either NewLordError City
getNewLord (City Nothing _ _) _ = Left NoCastle
getNewLord (City (Just (Protection (Castle (Just (Person _))) _)) _ _) _ = Left LordExists
getNewLord (City (Just (Protection (Castle (Nothing)) walls)) club houses) lord
  = Right (City (Just (Protection (Castle (Just lord)) walls)) club houses)

buildWalls :: City -> Walls -> Either String City
buildWalls city@(City (Just (Protection (Castle (lord)) Nothing)) club houses) walls
  | countCitizens city > 9 =
    (Right (City (Just (Protection (Castle (lord)) (Just walls))) club houses))
  | otherwise = Left "Not enough citizens"
    where
      countCitizens :: City -> Int
      countCitizens (City _ _ h)
        = foldl (\x (House citizens) -> x + fromEnum citizens + 1) 0 h
buildWalls _ _ = Left "No lord in city"
