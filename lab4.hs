
data Font = Consolas 
    | LucidaConsole 
    | SourceCodePro 
    deriving (Eq, Show)

data Figure 
    = Circle (Integer, Integer) Integer
  | Rectangle (Integer, Integer) (Integer, Integer)
  | Triangle (Integer, Integer) (Integer, Integer) (Integer, Integer)
  | Label (Integer, Integer) Font String
  deriving (Eq, Show)

getRectangle :: [Figure] -> [Figure]
getRectangle [] = []
getRectangle ((Rectangle (x1, y1) (x2, y2)) : fs) = Rectangle (x1, y1) (x2, y2) : getRectangle fs
getRectangle ((Circle (x1, y1) n) : fs) = Circle (x1, y1) n : getRectangle fs
getRectangle ((Triangle (x1, y1) (x2, y2) (x3, y3)) : fs) = Triangle (x1, y1) (x2, y2) (x3, y3) : getRectangle fs
getRectangle ((Label (x1, y1) fnt str) : fs) = Label (x1, y1) fnt str : getRectangle fs
