module Tree
  ( Tree(..)
  , isEmpty
  , size
  , find
  , insert
  , fromList
  , erase
  , testTree
  ) where

data Tree a =
    Leaf
  | Node [a] (Tree a) (Tree a)
    deriving (Show, Eq)

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

find :: Ord a => Tree a -> a -> Maybe (Tree a)
find Leaf _           = Nothing
find node@(Node keys left right) target
  | targetList == key = Just node
  | targetList < key  = find left target
  | otherwise         = find right target
    where
      key         = take 1 keys
      targetList  = [target]

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf target    = Node [target] Leaf Leaf
insert (Node keys left right) target
  | targetList == key = Node (keys ++ targetList) left right
  | targetList < key  = Node keys (insert left target) right
  | otherwise         = Node keys left (insert right target)
    where
      key         = take 1 keys
      targetList  = [target]

fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList (x:xs) = Node ([x] ++ filter (==x) xs)
  (fromList $ filter (<x) xs)
  (fromList $ filter (>x) xs)

erase :: Ord a => Tree a -> a -> Maybe (Tree a)
erase Leaf _         = Nothing
erase (Node keys left right) target
  | targetList < key  = erase left target
  | targetList > key  = erase right target
  | otherwise         = if length keys > 1
                        then Just (Node (drop 1 keys) left right)
                        else undefined
                          where
                            key         = take 1 keys
                            targetList  = [target]

testTree :: Bool
testTree = let t = fromList [1,2,3,4,5,6] in
  find t 4 == Just (Node [4] Leaf (Node [5] Leaf (Node [6] Leaf Leaf))) &&
  find t 7 == Nothing &&
  insert t 7 == Node [1] Leaf (Node [2] Leaf
    (Node [3] Leaf (Node [4] Leaf (Node [5] Leaf (Node [6] Leaf (Node [7] Leaf Leaf)))))) &&
  size t == 6
