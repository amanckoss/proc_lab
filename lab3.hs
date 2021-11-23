-- Лабораторна робота №3
-- студента групи КН-32
-- підгрупа 1
-- Гежа Михаїл
-- Варіант №8

-- Мета: Набути досвiду визначення та використання функцій вищого порядку.

-- Завдання: Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
-- застосуванням вбудованих функцiй вищого порядку. 

-- 1.8 Повторити n-кратно кожен елемент списку, напр. при n=2: "asd"⇒
-- [’a’, ’a’, ’s’, ’s’, ’d’, ’d’].

-- 2.8 Обчислити функцiю Ейлера φ(m).

-- Тести:
-- *Main> funk1 3 "gdflg"
-- "gdflggdflggdflg"

-- *Main> funk2 4 "gg"
-- "gggggggg"

-- *Main> funk3 34
-- 12

-- *Main> funk4 54
-- 18


import Text.XHtml.Transitional (multiple)

funk1 :: Int -> String -> String
funk1 1 str  = str
funk1 amount str =  concat $ replicate amount str

funk2 :: Int -> String -> String
funk2 1 str  = str
funk2 amount str = str ++ funk1 (amount - 1) str


-- funk3 :: Int -> Int
funk3 = helper 1 0
helper i amount num
    | i == num = amount
    | prime i = helper (i + 1) (amount + 1) num
    | otherwise = helper (i + 1) amount num

funk4 :: Int -> Int
funk4 n = length [x | x <- [1..n], coprime x n]

coprime :: Int -> Int -> Bool
coprime n m = gcd n m == 1

prime :: (Integral a) => a -> Bool
prime 1 = True
prime x = and [ x `mod` y /= 0 | y <- [2..(x-1)] ]

-- Висновок: В результаті виконання лабораторної роботи, ми набули досвіду визначення
-- та використання функцій вищого порядку.