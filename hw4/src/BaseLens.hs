{-# LANGUAGE Rank2Types     #-}

module BaseLens where

import           Data.Functor.Const    (Const (..), getConst)
import           Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set :: Lens' s a -> a -> s -> s
set l s = runIdentity . l (\_ -> Identity s)

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

view :: Lens' s a -> s -> a
view l s = getConst $ l Const s

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\b -> (b, x)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\a -> (x, a)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = set s <$> f (get s)

(.~) :: Lens s t a b -> b -> s -> t
l .~ b = runIdentity . l (\_ -> Identity b)

(%~) :: Lens s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)

(^.) :: s -> Lens s t a b -> a
s ^. l = getConst $ l Const s

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (^.l1) (^.l2)) 
                      (either 
                        (\s1 b -> Left  $ (l1.~b) s1)
                        (\s2 b -> Right $ (l2.~b) s2))

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f (s ^. l), (l %~ f) s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (s ^. l, (l %~ f) s)
