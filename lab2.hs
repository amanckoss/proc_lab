-- Лабораторна робота №2
-- студента групи КН-32
-- підгрупа 1
-- Гежи Михаіла
-- Варіант №8

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму
-- зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання: Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
-- застосуванням вбудованих функцiй.
-- Завдання 1.4: Переписати список справа налiво.

-- Завдання 2.4: Перемiшування списку з n елементiв: пiсля першого розмiстити останнiй,
-- пiсля другого – передостаннiй тощо.

-- Тести:

-- *Main> funk_main "aaabbcaadddd"
-- [('a',3),('b',2),('c',1),('a',2),('d',4)]

-- *Main> funk1_2 "2a3b"
-- [('a',2),('b',3)]

-- *Main> funk2_1 20 40
-- 20

-- *Main> funk2_2 20 5      
-- '5'

import Data.Char

funk_main :: [Char] -> [(Char, Int)]
funk_main [] = []
funk_main ctr = funk1 ctr [] '`' 0

funk1 :: [Char] -> [(Char, Int)] -> Char -> Int -> [(Char, Int)]
funk1 str newStr var amount
    | var == '`' = funk1 (tail str) newStr (head str) (amount + 1)
    | length str == 0 = newStr ++ [(var, amount)]
    | var == head str = funk1 (tail str) newStr var (amount + 1)
    | otherwise       =  funk1 (tail str) (newStr ++ [(var, amount)]) (head str) 1

funk1_2 :: String -> [(Char,Int)]
funk1_2 [] = []
funk1_2 (x:y:ls) = [(y,digitToInt x)] ++ funk1_2 ls

funk2_1 :: Integer -> Integer -> Integer
funk2_1 0 b = b
funk2_1 a b = funk2_1 (b `mod` a) a


funk2_2 :: Int -> Int -> Char
funk2_2 0 b = intToDigit b
funk2_2 a b = funk2_2 (b `mod` a) a

-- Висновок: В результаті виконання лабораторної роботи, ми набули досвіду визначення рекурсивниї функцій,
-- навчилися використовувати механізм зіставлення зі зразком та попрацювали з кортежами та списками.
