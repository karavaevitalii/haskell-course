module Tree
  ( Tree(..)
  , isEmpty
  , size
  , find
  , insert
  , fromList
  ) where

data Tree a =
  Leaf
  | Node [a] (Tree a) (Tree a)
    deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty Leaf  = True
isEmpty _     = False

size :: Tree a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

find :: (Ord a) => Tree a -> a -> Maybe (Tree a)
find Leaf _           = Nothing
find node@(Node keys left right) target
  | targetList == key = Just node
  | targetList < key  = find left target
  | otherwise         = find right target
    where
      key         = take 1 keys
      targetList  = [target]

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf target    = Node [target] Leaf Leaf
insert (Node keys left right) target
  | targetList == key = Node (keys ++ targetList) left right
  | targetList < key  = Node keys (insert left target) right
  | otherwise         = Node keys left (insert right target)
    where
      key         = take 1 keys
      targetList  = [target]

fromList :: (Ord a) => [a] -> Tree a
fromList []     = Leaf
fromList (x:xs) = Node ([x] ++ filter (==x) xs)
  (fromList $ filter (<x) xs)
  (fromList $ filter (>x) xs)