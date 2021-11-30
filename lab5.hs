import Data.Char
-- Лабораторна робота №5
-- студентки групи КН-32 підгрупи 1
-- Гежа Михаїл
-- Варіант №7

--Завдання 1.Переписати список справа наліво.
funk1 :: Int -> [a] -> [a]
funk1 1 str  = str
funk1 amount str = str ++ funk1 (amount - 1) str

vyvod :: IO()
vyvod = do
-- a) введення з клавіатури
    putStrLn "Input amount:"
    amount<-getLine
    putStrLn "Input str:"
    str<-getLine
    putStrLn(funk1 (digitToInt (head amount)) str)
--б) введення даних з файлу
    strL<-readFile "input.txt"
    str <- hReadLine  strL
    amount <- hReadLine  strL
    putStrLn(funk1 (digitToInt (head amount)) str)
--в) виведення результатів на екран
    putStrLn "Input amount:"
    amount<-getLine
    putStrLn "Input str:"
    str<-getLine
    writeFile "output.txt" (funk1 (digitToInt (head amount)) str)
--г) виведення результатів у файл
    strL<-readFile "input.txt"
    str <- hReadLine  strL
    amount <- hReadLine  strL
    writeFile "output.txt" (funk1 (digitToInt (head amount)) str)

-- Висновок: Ознайомились з модульною органiзацiєю програм та засобами введення-виведення.