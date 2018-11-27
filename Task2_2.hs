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
diagonal :: [[a]] -> [a]
diagonal x = zipWith (!!) x [0..]

-- Фильтр для всех элементов, не соответствующих предикату
getRes fun a lst | (fun a) = a:lst
                 | otherwise = lst

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot fun lst = foldr (getRes fun) [] lst

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = (filterNot ( == e) lst) /= []

-- Список чисел в диапазоне [from, to) с шагом step
addNew a b step res | (a < b) = a:(addNew (a+step) b step res)
                    | (a >= b) = b:res
                    | otherwise = res

rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = addNew from to step []

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append [] b = b
append a [] = a
append a b = foldr f b a 
           where f t h = t:h

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)

groups :: [a] -> Integer -> [[a]]
groups [] _ = []
groups lst n = (take (fromIntegral n) lst) : (groups  (drop (fromIntegral n) lst) n)
