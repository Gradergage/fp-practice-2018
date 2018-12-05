module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

get1 (FourOf a _ _ _) = a
get2 (FourOf _ a _ _) = a
get3 (FourOf _ _ a _) = a
get4 (FourOf _ _ _ a) = a

instance Functor (FourOf) where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Monad FourOf where
    return a = FourOf a a a a
    (>>=) (FourOf a b c d) f = FourOf (get1 (f a)) (get2 (f b)) (get3 (f c)) (get4 (f d))

instance Applicative FourOf where
    pure = return
    (<*>) (FourOf f1 f2 f3 f4) (FourOf a b c d) = FourOf (f1 a) (f2 b) (f3 c) (f4 d)