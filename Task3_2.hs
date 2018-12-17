module Task3_2 where

import Todo(todo)
data ReverseList a = RNil | RCons (ReverseList a) a

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance Foldable ReverseList where
    foldr f z RNil = z
    foldr f z (RCons t h) = f h (foldr f z t)

rlistToList :: ReverseList a -> [a]
rlistToList = foldl (\t h -> h:t) []

listToRList :: [a] -> ReverseList a
listToRList = foldl (\t h -> RCons t h) RNil

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) _    RNil = False
    (==) RNil _    = False
    (==) (RCons t h) (RCons t' h') | h == h' = t == t'
                                   | otherwise = False

instance (Ord a) => Ord (ReverseList a) where
    RNil `compare` RNil = EQ
    RNil `compare` (RCons t h) = LT
    (RCons t h) `compare` RNil = GT
    (RCons t h) `compare` (RCons t' h')  | h < h' = LT
                                         | h > h' = GT
                                         | otherwise = t `compare` t'

instance (Show a) => Show (ReverseList a) where
    show a = "[" ++ showBody a ++ "]" where
                    showBody RNil = ""
                    showBody (RCons RNil h) = show h
                    showBody (RCons t h) = showBody t ++ "," ++ show h


instance Semigroup (ReverseList a) where
    (<>) t t' = foldr (\x s -> RCons s x) t' t

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend = (<>)

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons t h) = RCons (f <$> t) (f h)

