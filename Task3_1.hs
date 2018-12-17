module Task3_1 where

import Task2_2
import Prelude hiding (toInteger, fromInteger)
data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize num = normalize' num Zero where
                normalize' (Succ (Pred n)) acc = normalize' n acc -- выкидываем взаимоисключающие операции
                normalize' (Pred (Succ n)) acc = normalize' n acc
                normalize' (Succ n) acc = normalize' n (Succ acc)
                normalize' (Pred n) acc = normalize' n (Pred acc)
                normalize' Zero acc = acc

toInteger :: WeirdPeanoNumber -> Int
toInteger num = 
        let num' = normalize num in
                toInteger' num' 0 where
                toInteger' Zero a = a
                toInteger' (Succ n) a = toInteger' n (a + 1)
                toInteger' (Pred n) a = toInteger' n (a - 1)

fromInteger :: Int -> WeirdPeanoNumber
fromInteger num | num > 0 = Succ (fromInteger (num - 1))
                | num < 0 = Pred (fromInteger (num + 1))
                | otherwise = Zero

instance Eq WeirdPeanoNumber where
    (==) a b = cmpr (normalize a) (normalize b) where
               cmpr Zero Zero = True
               cmpr Zero _ = False
               cmpr _ Zero = False
               cmpr (Succ a) (Pred b) = False
               cmpr (Pred a) (Succ b) = False
               cmpr (Succ a) (Succ b) = cmpr a b
               cmpr (Pred a) (Pred b) = cmpr a b
    (/=) a b = not (a == b)

instance Show WeirdPeanoNumber where
    show Zero = "Zero"
    show (Succ a) = "(Succ " ++ show a ++ ")"
    show (Pred a) = "(Pred " ++ show a ++ ")"

instance Enum WeirdPeanoNumber where
    toEnum x = fromInteger x
    fromEnum x = toInteger x

instance Bounded WeirdPeanoNumber where
    minBound = fromInteger minBound
    maxBound = fromInteger maxBound

instance Ord WeirdPeanoNumber where
    a `compare` b | a == b = EQ
                  | toInteger a > toInteger b = GT
                  | otherwise = LT
    (<) a b = toInteger a < toInteger b
    (<=) a b = toInteger a <= toInteger b
    (>) a b = not (a <= b)
    (>=) a b = not (a < b)

instance Num WeirdPeanoNumber where
    (+) a Zero = a
    (+) Zero b = b 
    (+) a b = fromInteger (toInteger a + toInteger b)
    (*) _ Zero = Zero
    (*) Zero _ = Zero
    (*) a b = fromInteger (toInteger a * toInteger b)
    negate a = (Pred Zero) * a
    abs a = let num' = normalize a in
            if (toInteger num') > 0 then num'
            else fromInteger((toInteger num') * (-1))
  