module Task2_1 where

import Todo(todo)
import Prelude hiding(lookup)
-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v  = Empty
                | Node Integer v (TreeMap v) (TreeMap v)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty


-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty _ = False
contains (Node key _ l r) k | k > key = contains r k
                            | k < key = contains l k
                            | k == key = True

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ Empty = error "Empty"
lookup k (Node key value l r) | k > key = lookup k r
                              | k < key = lookup k l
                              | k == key = value

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Empty = Node k v Empty Empty
insert (k, v) (Node key value l r) | k > key = Node key value l (insert (k, v) r) 
                                   | k < key = Node key value (insert (k, v) l) r
                                   | k == key = Node k v l r

-- Удаление элемента по ключу
minNode :: TreeMap v -> (Integer, v)
minNode (Node key value Empty _) = (key, value)
minNode (Node key value l _) = minNode l

careTree :: TreeMap v -> TreeMap v -> TreeMap v
careTree l r =
    let (k, v) = minNode r in Node k v l (remove k r) 

remove :: Integer -> TreeMap v -> TreeMap v
remove _ Empty = error "Empty"
remove i (Node key value l r) | i > key = remove i r
                              | i < key = remove i l
                              | i == key = case (l, r) of
                                            (Empty, Empty) -> Empty
                                            (l , Empty) -> l
                                            (Empty, r) -> r
                                            (l, r) -> careTree l r

-- Поиск ближайшего снизу ключа относительно заданного

abs' :: Integer -> Integer
abs' n | n >= 0    = n
      | otherwise = -n

getKey :: TreeMap v-> Integer
getKey (Node key _ _ _) = key

getKV :: TreeMap v-> (Integer, v)
getKV (Node key value _ _) = (key, value)

minKey Empty = error "Empty"
minKey (Node key value l Empty) = getKV l
minKey (Node key value Empty r) = getKV r
minKey (Node key value Empty Empty) = (key, value)
minKey (Node key value l r) | abs'((key - getKey l)) <= abs'((key -getKey r)) = getKV l
                            | otherwise = getKV r

nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE k Empty = error "Empty"
nearestLE k node@(Node key value l r) | k > key = nearestLE k r
                                      | k < key = nearestLE k l
                                      | k == key = minKey node

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Empty = []
listFromTree (Node key value l r) = concat [listFromTree l, [(key, value)], listFromTree r]


-- Поиск k-той порядковой статистики дерева 
sizeT :: TreeMap v -> Integer
sizeT Empty = 0
sizeT (Node _ _ l r) = 1 + sizeT l + sizeT r

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Empty = error "Empty"
kMean i (Node key value l r) | i == sizeL = (key, value)
                             | i < sizeL = kMean i l
                             | otherwise = kMean (i - sizeL - 1) r
                             where sizeL = sizeT l
