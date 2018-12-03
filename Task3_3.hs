module Task3_3 where


newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Для моноида:

-- Для осуществления этого, согласно сигнатуре,
-- PSet должен иметь нейтральное значение 
-- операция группировки с использованием логического ИЛИ и нейтрального значения False
-- обеспечивает соблюдение основного закона моноида, т. к
-- False || a === a || False === а
-- Либо же для логического И и нейтрального значения True
-- True && a === a && True === а

instance Semigroup (PSet t) where
    (<>) (PSet a) (PSet b) = PSet (\x -> a x || b x)

instance Monoid (PSet a) where
    mempty = PSet (\_ -> False) -- нейтральное значение моноида
  -- таким образом для PSet выполняется a `mappend` mempty === mempty `mappend` a === a
    mappend = (<>)

-- для True и &&
newtype PSet' a = PSet'(a -> Bool)

instance Semigroup (PSet' t) where
    (<>) (PSet' a) (PSet' b) = PSet' (\x -> a x && b x)

instance Monoid (PSet' a) where
    mempty = PSet' (\_ -> True)
    mappend = (<>)


-- Для функтора:
-- согласно сигнатуре функции fmap :: (a -> b) -> f a -> f b
-- стандартный функтор является ковариантным
-- PSet(a -> Bool) является контравариантным относительно а
-- Для того чтобы корректно использовать функтор нужно:
-- либо изменить тип PSet так, чтобы он тоже стал ковариантным;

newtype СontraPSet a = СontraPSet (Bool -> a)

instance Functor СontraPSet where
    fmap f (СontraPSet a) = СontraPSet (f . a)
--                                       ^ композиция функций

-- либо применять контравариантый функтор fmap :: (a -> b) -> f b -> f a
-- относительно стандартного PSet:

class ContraFunctor f where
    contrmap :: (a -> b) -> f b -> f a

instance ContraFunctor PSet where
    contrmap f (PSet a) = PSet (a . f)



