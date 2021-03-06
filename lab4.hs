-- Лабораторна робота №4
-- студента групи КН-32
-- підгрупа 1
-- Гежа Михаїл
-- Варіант №8

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Тести: 
-- *Main> funk1 [(Book "User1" "dsfdfs"),(Book "User2" "dsfdfs"),(Book "User3" "dsfdfs")] "User1"         
-- [Book "User1" "dsfdfs"]

-- *Main> funk1 [(Book "User1" "dsfdfs"),(Book "User2" "dsfdfs"),(Book "User1" "dhjhksfdfs")] "User1"
-- [Book "User1" "dsfdfs",Book "User1" "dhjhksfdfs"]


data Publication
    = Book String Stringу
   | Article String String String String String String String
   | Tez String String String String String String
  deriving (Eq, Show)

funk1 :: [Publication] -> String -> [Publication]
funk1 [] k = []
funk1 ((Book x y) : fs) k = if x == k
  then Book x y : funk1 fs k
  else funk1 fs k
funk1 ((Article a b c d e f g) : fs) k = if a == k
  then Article a b c d e f g : funk1 fs k
  else funk1 fs k
funk1 ((Tez a b c d e f) : fs) k = if a == k
  then Tez a b c d e f : funk1 fs k
  else funk1 fs k

-- Висновок: Під час виконання лабораторної роботи, ми ознайомилися та 
-- імплементували класи типів мови Haskell. Також ознайомилися з системою
-- типів та класів типів, визначили власні функції для нового типу. 
