module TypesHierarchy
    ( stringSum
    , Optional (..)
    , NonEmpty (..)
    ) where


import           Data.Foldable (toList)
import           Text.Read     (readMaybe)

stringSum :: String -> Maybe Int
stringSum str = fmap sum $ sequence $ readMaybe <$> words str

newtype Optional a = Optional (Maybe (Maybe a))
    deriving Show

instance Functor Optional where
    fmap _ (Optional Nothing)         = Optional Nothing
    fmap _ (Optional (Just Nothing))  = Optional $ Just Nothing
    fmap f (Optional (Just (Just a))) = Optional $ Just $ Just $ f a

instance Applicative Optional where
    pure a = Optional $ Just $ Just a

    (Optional Nothing)          <*> _   = Optional Nothing
    (Optional (Just Nothing))   <*> _   = Optional $ Just Nothing
    (Optional (Just (Just f)))  <*> op  = fmap f op

instance Monad Optional where
    return = pure

    Optional Nothing            >>= _ = Optional Nothing
    Optional (Just Nothing)     >>= _ = Optional $ Just Nothing
    Optional (Just (Just a))    >>= f = f a

instance Foldable Optional where
    foldr _ z (Optional Nothing)         = z
    foldr _ z (Optional (Just Nothing))  = z
    foldr f z (Optional (Just (Just a))) = f a z

instance Traversable Optional where
    traverse _ (Optional Nothing)         = pure $ Optional Nothing
    traverse _ (Optional (Just Nothing))  = pure $ Optional $ Just Nothing
    traverse f (Optional (Just (Just a))) = Optional . Just . Just <$> f a

data NonEmpty a = a :| [a] deriving Show

instance Functor NonEmpty where
    fmap f (x:|xs) = f x :| map f xs

instance Applicative NonEmpty where
    pure a = a:|[]

    (f:|fs) <*> (x:|xs) = f x :| drop 1 [fs' xs' | fs' <- f:fs, xs' <- x:xs]

instance Monad NonEmpty where
    return = pure

    (x:|xs) >>= f = y :| (ys ++ ys')
      where
        y :| ys = f x
        ys' = xs >>= toList . f

instance Foldable NonEmpty where
    foldr f z (x:|xs) = x `f` foldr f z xs

instance Traversable NonEmpty where
    traverse f (x:|xs) = (:|) <$> f x <*> traverse f xs
