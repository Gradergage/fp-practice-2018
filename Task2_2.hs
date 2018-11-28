module Task2_2 where

import Todo(todo)
import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []

sizeL :: [a] -> Integer
sizeL lst = foldl f 0 lst where
            f acc element = acc + 1
-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst 
            where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (h:t) = (f h) : (map f t)

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

justify m lst = case m of
                     Nothing -> lst
                     Just v -> (v: lst)

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr justify [] lst

-- Диагональ матрицы
getElem n lst = foldl f 0 lst where
                f acc elem = lst !! n

diagonal :: [[a]] -> [a]

diagonal x = unfoldr f 0 where
             f ptr | (ptr < sizeL x) = Just(((x !! (fromIntegral ptr)) !! (fromIntegral ptr)), ptr + 1)
                   | otherwise = Nothing
-- diagonal x = zipWith (!!) x [0..]
-- diagonal x = unfoldr f x where
          --   f ptr



-- Фильтр для всех элементов, не соответствующих предикату
getRes fun a lst | (fun a) = a:lst
                 | otherwise = lst

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot fun lst = foldr (getRes fun) [] lst

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = (filterNot ( == e) lst) /= []

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from where
                       f ptr | ptr < to = Just (ptr, ptr + step)
                             | otherwise = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append [] b = b
append a [] = a
append a b = foldr f b a 
           where f t h = t:h



-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
splitList :: Integer -> [a] -> ([a], [a])
splitList 0 t = ([], t)
splitList _ [] = ([], [])
splitList n (h:t) | n < 0 = ([], (h:t))
                  | otherwise = ((h:a), b)
                  where (a, b) = splitList (n - 1) t

groups :: [a] -> Integer -> [[a]]
groups t n = unfoldr f t where
             f [] = Nothing
             f ptr =
                   let snd' = snd(splitList n ptr)
                       fst' = fst(splitList n ptr) in
                   if (sizeL snd' == 0) && (sizeL fst' == 0) then Nothing
                   else Just (fst',snd')