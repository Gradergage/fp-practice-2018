module Task3_1 where

import Task2_2
data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize num = normalize' num Zero where
                normalize' (Succ (Pred n)) acc = normalize' n acc -- выкидываем взаимоисключающие операции
                normalize' (Pred (Succ n)) acc = normalize' n acc
                normalize' (Succ n) acc = normalize' n (Succ acc)
                normalize' (Pred n) acc = normalize' n (Pred acc)
                normalize' Zero acc = acc

myToInteger :: WeirdPeanoNumber -> Int
myToInteger num = 
        let num' = normalize num in
                myToInteger' num' 0 where
                myToInteger' Zero a = a
                myToInteger' (Succ n) a = myToInteger' n (a + 1)
                myToInteger' (Pred n) a = myToInteger' n (a - 1)

myFromInteger :: Int -> WeirdPeanoNumber
myFromInteger num | num > 0 = Succ (myFromInteger (num - 1))
                  | num < 0 = Pred (myFromInteger (num + 1))
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
    toEnum x = myFromInteger x
    fromEnum x = myToInteger x

instance Bounded WeirdPeanoNumber where
    minBound = myFromInteger minBound
    maxBound = myFromInteger maxBound

instance Ord WeirdPeanoNumber where
    a `compare` b | a == b = EQ
                  | myToInteger a > myToInteger b = GT
                  | otherwise = LT
    (<) a b = myToInteger a < myToInteger b
    (<=) a b = myToInteger a <= myToInteger b
    (>) a b = not (a <= b)
    (>=) a b = not (a < b)

instance Num WeirdPeanoNumber where
    (+) a Zero = a
    (+) Zero b = b 
    (+) a b = myFromInteger (myToInteger a + myToInteger b)
    (*) _ Zero = Zero
    (*) Zero _ = Zero
    (*) a b = myFromInteger (myToInteger a * myToInteger b)
    negate a = (Pred Zero) * a
    abs a = let num' = normalize a in
            if (myToInteger num') > 0 then num'
            else myFromInteger((myToInteger num') * (-1))

    signum Zero = Zero
    signum (Succ (Pred a)) = signum a
    signum (Pred (Succ a)) = signum a
    signum (Succ a) = Succ Zero
    signum (Pred a) = Pred Zero

    fromInteger = myFromInteger . fromIntegral

instance Real WeirdPeanoNumber where
    toRational = toRational . myToInteger

quot' a b = if (a - b >= Zero)
                then (quot (a - b)  b) + 1 
                else Zero
rem' a b = if (a - b >= Zero)
                then rem' (a - b) b
                else a

instance Integral WeirdPeanoNumber where
    toInteger Zero = 0
    toInteger (Succ a) = toInteger a + 1
    toInteger (Pred a) = toInteger a - 1
    quotRem a b = let sign = signum a * signum b
                      a' = abs a
                      b' = abs b
                      in if(b == Zero) then error "Can't divide zero"
                                       else (sign *(quot' a' b'), sign * (rem' a' b'))