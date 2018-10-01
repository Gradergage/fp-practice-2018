module Task1_2 where

import Todo(todo)
import Prelude hiding (sin, cos, gcd)

-- факториал числа (для последующих)
factorial x = if x <= 1 then 1 else x * factorial (x - 1)

--false - cos, true - sin
taylorElement :: Double->Double->Bool->Double
taylorElement x n t = if t then (((-1) ** n) * (x ** (2*n+1))) / factorial(2*n+1) 
                      else  (((-1) ** n) * (x ** (2*n))) / factorial(2*n)
--false - cos, true - sin
taylorRecursion :: Double -> Double->Bool->Double
taylorRecursion  x n t | x == 0 || n == -1 = 0
          | otherwise = (taylorElement x n t) + (taylorRecursion  x (n-1) t)

absXY x y | (abs x) > (abs y) = (abs x) - (abs y)
          | (abs x) <= (abs y) = (abs y) - (abs x)

frac :: Double -> Double->Double
frac x y = absXY (x/y) (fromIntegral (floor (x/y)))

--Попытка сокращать значение углов до предела от +- нуля до пи
--Но если пи подавать через аргумент и умножать на коэффициент, например (10*pi) то пара последних разрядов побъется и пи
--перестанет идентифицироваться как пи при рекурсивном вычитании, поэтому ниже функция sin2 закомменчена,
--в ней обрабатывались базовые значения углов, в которых нужны точные значения, то есть pi/6, pi/2. Но даже если послать
--10*pi, то значение не пройдет корректную конвертацию из-за того что операции с pi округляются внутри разрядной сетки,
--поэтому функции sin и cos работают с приближенными значениями углов в заданных пределах
convertArgument :: Double->Double
convertArgument x |  x>0 && x>(2*pi)= convertArgument (x - 2*pi)
                     |  x<0 && x<(-(2*pi))= convertArgument (x + 2*pi)
                     |  x>0 && x<(2*pi) && x>(pi) = x-2*pi
                     |  x<0 && x>(-2*pi) && x<(-pi) = x+2*pi
                     |  x==0 = 0
                     | otherwise = x

--Версия sin, в которой обрабатываются случаи точных значений
sin2 x = 
    let p = (convertArgument x) in
    if p==0 || p==pi then 0
    else if (p==(pi/2)) then 1
    else if (p==(3*(pi/2))) then -1
    else if (p==((pi/6)) || p==((pi-pi/6)) ) then 0.5
    else if (p==((-pi/6)) || p==((-pi+pi/6))) then -0.5
    else taylorRecursion p 100 True 


-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x =  
    let p = (convertArgument x) in
    taylorRecursion p 100 True 

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = 
    let p = (convertArgument x) in
    taylorRecursion x 100 False

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = if y==0 then x
          else gcd y (x `mod` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = ((floor (sqrt (fromIntegral (to - 1)))) - (ceiling (sqrt (fromIntegral from)))) >= 0

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | month < 1 || month > 12 || day < 1 || year == 0 = False
  | month == 2 = day < 29 || (leap && day == 29)
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = day < 32
  | otherwise = day < 31
  where
    leap = (year' `mod` 4 == 0 && year' `mod` 100 /= 0) || year' `mod` 400 == 0
    year' | year > 0     = year
          | otherwise = year + 1

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = if y==0 then 1
               else x*(pow x (y-1))

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x == 0 = False
          | x == 1 = False
          | otherwise = not (any (\i -> (x `mod` i) == 0) [2..floor(sqrt(fromIntegral x))])

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
