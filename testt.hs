f1 :: Integer -> Integer -> Integer -> Integer -> Bool 
f1 c1 c2 r1 r2
  | abs(c1 - c2)  < abs(r1 - r2) = True
  |  otherwise = False


f2 :: (Integer, Integer, Integer, Integer) -> Bool
f2 (c1, c2, r1, r2)
  | abs(c1 - c2)  < abs(r1 - r2) = True
  |  otherwise = False