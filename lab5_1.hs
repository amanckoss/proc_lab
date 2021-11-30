-- Лабораторна робота №5
-- студентки групи КН-32 підгрупи 1
-- Євдокимов Нікіта
-- Варіант №11  

--Завдання 1.Переписати список справа наліво.

funk1 :: String  -> Char -> String
funk1 [] ch = []
funk1 (x:xs) ch
    | length xs > 1 = x : ch : funk1 (tail xs) ch
    | otherwise = funk1 [] ch

vyvod :: IO()
vyvod = do
-- a) введення з клавіатури
    putStrLn "Input str:"
    str<-getLine
    putStrLn "Input: char"
    char<-getLine
    putStrLn(funk1 str (head char))
--б) введення даних з файлу
    contents <- readFile "file.txt"
--в) виведення результатів на екран
    putStrLn "Input str:"
    str<-getLine
    putStrLn "Input: char"
    char<-getLine
    putStrLn(funk1 str (head char))
--г) виведення результатів у файл
    writeFile "output.txt" (funk1 str (head char))

    
-- Висновок: Ознайомились з модульною органiзацiєю програм та засобами введення-виведення.