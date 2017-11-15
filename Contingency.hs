{-
	Plantilla de c�digo para el proyecto del curso 2017 de "Electiva - Programaci�n Funcional" para
	las carreras de Ingenier�a y Licenciatura en Inform�tica de la FIT (UCU).

	Por Leonardo Val.
-}
module Contingency where

import Data.Maybe
import Data.List
import Data.Char

-- Datas ------------------------------------------------------------------------------------------
data ContingencyPlayer = PlayerTrue | PlayerFalse deriving (Eq, Show, Enum)

data ContingencyGame   = Board [Piece] [String] -- 7*7 = 49
instance Show ContingencyGame where
  show b = showBoard b

data Direction = Vertical | Horizontal deriving (Show, Eq)

data ContingencyAction = Action String (Int,Int) Direction

instance Show ContingencyAction where
  show a = showAction a

data Piece = Empty | Hidden Bool | True_ | False_ | OpAnd Direction
           | OpOr Direction | OpIff Direction | OpXor Direction deriving Eq
instance Show Piece where
  show Empty = "."
  show (Hidden _) = "0"
  show True_ = "t"
  show False_ = "f"
  show (OpAnd _) = "&"
  show (OpOr _) = "|"
  show (OpIff _) = "i"
  show (OpXor _) = "x"

-- Game logic stub ---------------------------------------------------------------------------------

type Operador = (Bool -> Bool -> Bool)
iff, xor :: Operador
iff p q = (p && q) || ((not p) && (not q))
xor p q = ((not p) && q) || (p && (not q))

beginning :: IO ContingencyGame
beginning = error "beginning has not been implemented!" --TODO

--9 AND de dos operandos, 9 OR de dos operandos, 6 XOR de dos operandos, 6 IFF (si y sólo si) de dos operandos
emptyBoard :: ContingencyGame
emptyBoard = Board (replicate 49 Empty) ((replicate 9 "and") ++ (replicate 9 "or") ++ (replicate 6 "xor") ++ (replicate 6 "iff"))

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer g@(Board b _)
  | isFinished g = Nothing
  | otherwise = if even (countOp b) then (Just PlayerTrue) else (Just PlayerFalse)

actions :: ContingencyGame -> ContingencyPlayer -> String -> [ContingencyAction]
actions g@(Board b _) p str = foldl (fun str) [] (emptyPositions g)
                            where fun str l (x,y)
                                    | elem (x-1,y) adj && elem (x+1,y) adj = if (elem (x,y-1) adj && elem (x,y+1) adj)
                                                                             then (Action str (x,y) Horizontal):((Action str (x,y) Vertical):l)
                                                                             else (Action str (x,y) Horizontal):l
                                    | elem (x,y-1) adj && elem (x,y+1) adj = if (elem (x-1,y) adj && elem (x+1,y) adj)
                                                                             then (Action str (x,y) Horizontal):((Action str (x,y) Vertical):l)
                                                                             else (Action str (x,y) Vertical):l
                                    | otherwise = l
                                      where adj = fullPositions g

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState g@(Board b bag) p (Action str (x,y) d) = let i = toIndex (x,y) in
                                               if (b!!i) == Empty && (activePlayer g) == (Just p) then
                                                 do return (Board (insertAt b (piece str d) i) bag)
                                               else error "!!!"

isFinished :: ContingencyGame -> Bool
isFinished g@(Board b bag) = (countOp b == 30) || (null bag)

score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "score has not been implemented!" --TODO

showBoard :: ContingencyGame -> String
showBoard (Board b bag) = (unlines $ map concat $ chunksOf 7 (map show b)) ++ (unwords bag)

showAction :: ContingencyAction -> String
showAction (Action "iff" c Vertical) = "i " ++ (show c)
showAction (Action "iff" c Horizontal) = "I " ++ (show c)
showAction (Action "xor" c Vertical) = "x " ++ (show c)
showAction (Action "xor" c Horizontal) = "X " ++ (show c)
showAction (Action "and" c Vertical) = "a " ++ (show c)
showAction (Action "and" c Horizontal) = "A " ++ (show c)
showAction (Action "or" c Vertical) = "o " ++ (show c)
showAction (Action "or" c Horizontal) = "O " ++ (show c)

readAction :: String -> ContingencyAction
readAction str
    | l!!0 == "i" = (Action "iff" c Vertical)
    | l!!0 == "I" = (Action "iff" c Horizontal)
    | l!!0 == "x" = (Action "xor" c Vertical)
    | l!!0 == "X" = (Action "xor" c Horizontal)
    | l!!0 == "a" = (Action "and" c Vertical)
    | l!!0 == "A" = (Action "and" c Horizontal)
    | l!!0 == "o" = (Action "or" c Vertical)
    | l!!0 == "O" = (Action "or" c Horizontal)
    | otherwise = error "!!!"
      where l = words str
            c = read (l!!1)

-- Match controller --------------------------------------------------------------------------------
type Agent = ContingencyGame -> ContingencyPlayer -> IO ContingencyAction

consoleAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
consoleAgent _ _ = error "consoleAgent has not been implemented!"

randomAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
randomAgent _ _ = error "randomAgent has not been implemented!"

runMatch :: (ContingencyPlayer, ContingencyPlayer) -> ContingencyGame -> IO (Int, Int)
runMatch players@(agTrue, agFalse) g = do
  putStrLn (showBoard g);
  let active = fromJust (activePlayer g);
  nextAction <- (consoleAgent g active);
  nextBoard <- (nextState g active nextAction);
  if
    (isFinished g)
  then
    return (score g agTrue, score g agFalse)
  else
    runMatch players nextBoard

runOnConsole :: IO (Int, Int)
runOnConsole = do
  board <- beginning
  runMatch (PlayerTrue, PlayerFalse) board

-- Auxiliar ---------------------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = map (map snd) $ groupBy groupFunction (zip [0..] xs)
                where groupFunction (i1,_) (i2,_) = (div i1 n) == (div i2 n)

countOp :: [Piece] -> Int
countOp b = sum $ map fun b
                    where fun Empty = 0
                          fun (Hidden _) = 0
                          fun True_ = 0
                          fun False_ = 0
                          fun _ = 1

insertAt :: [a] -> a -> Int -> [a]
insertAt l e i = (take i l) ++ (e:(drop (i+1) l))

piece :: String -> Direction -> Piece
piece str d
  | str == "iff" = (OpIff d)
  | str == "xor" = (OpXor d)
  | str == "or"  = (OpOr d)
  | str == "and" = (OpAnd d)
  | otherwise = error "!!!"

toIndex :: (Int,Int) -> Int
toIndex (x,y) = 7*x+y

fromIndex :: Int -> (Int,Int)
fromIndex i = (div i 7, mod i 7)

emptyPositions :: ContingencyGame -> [(Int,Int)]
emptyPositions (Board b _) = [fromIndex i | i<-(elemIndices Empty b)]

fullPositions :: ContingencyGame -> [(Int,Int)]
fullPositions (Board b _) = [fromIndex (fromJust (elemIndex x b))| x<-b,x/=Empty]

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(a,b) | a<-[x-1..x+1],b<-[y-1..y+1],a<7,a>=0,b>=0,b<7,(a,b)/=(x,y)]

deleteAt :: [a] -> Int -> [a]
deleteAt l i = (take i l) ++ (drop (i+1) l)
