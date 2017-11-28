{-
	C�digo para el proyecto del curso 2017 de "Electiva - Programaci�n Funcional" para
	las carreras de Ingenier�a y Licenciatura en Inform�tica de la FIT (UCU).

	Por Eduardo Perez y German Rodriguez.
-}
module Contingency where

import Data.Maybe
import Data.List
import Data.Char
import System.Random

-- Datas ------------------------------------------------------------------------------------------
data ContingencyAction = Action String (Int,Int) Direction
instance Show ContingencyAction where
  show a = showAction a

data ContingencyGame   = Board [Piece] [String] String -- 7*7 = 49
instance Show ContingencyGame where
  show b = showBoard b

data ContingencyPlayer = PlayerTrue | PlayerFalse deriving (Eq, Show, Enum)

data Direction = Horizontal | Vertical deriving (Eq, Show)

data Piece = Empty | True_ | False_ | Hidden Bool | OpAnd Direction
           | OpOr Direction | OpIff Direction | OpXor Direction deriving Eq
instance Show Piece where
  show Empty              = "."
  show (Hidden _)         = "0"
  show True_              = "t"
  show False_             = "f"
  show (OpIff Horizontal) = "i"
  show (OpIff Vertical)   = "I"
  show (OpXor Horizontal) = "x"
  show (OpXor Vertical)   = "X"
  show (OpAnd Horizontal) = "a"
  show (OpAnd Vertical)   = "A"
  show (OpOr Horizontal)  = "o"
  show (OpOr Vertical)    = "O"

-- Game logic stub --------------------------------------------------------------------------------

type Operador = (Bool -> Bool -> Bool)
iff, xor :: Operador
iff p q = (p && q) || ((not p) && (not q))
xor p q = ((not p) && q) || (p && (not q))

operadores = concat $ map (\(x,y) -> replicate x y) $ take 4 $ zip (cycle [9,6]) (cycle ["and","xor","or","iff"])

emptyBoard :: ContingencyGame
emptyBoard = Board (replicate 49 Empty) operadores ""

beginning :: IO ContingencyGame
beginning =
  do list <- randomBools $ concat $ [replicate 8 x | x<-[True,False]]
     i <- randomRIO (0,length operadores - 1)
     let board = [Hidden (list!!0), Empty, Empty, Empty, Empty, Empty, Empty, Empty, Hidden (list!!1), Empty,
                  Hidden (list!!2), Empty, Hidden (list!!3), Empty, Empty, Empty, Hidden (list!!3), Empty,
                  Hidden (list!!4), Empty, Empty, Empty, Hidden (list!!5), Empty, Hidden (list!!6), Empty,
                  Hidden (list!!7), Empty, Empty, Empty, Hidden (list!!8), Empty, Hidden(list!!9), Empty,
                  Empty, Empty, Hidden (list!!10), Empty, Hidden (list!!11), Empty, Hidden (list!!12),
                  Empty, Hidden (list!!13), Empty, Empty, Empty, Empty, Empty, Hidden (list!!14)]
     return (Board board (deleteAt operadores i) (operadores!!i))

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer g@(Board b _ _)
  | isFinished g = Nothing
  | otherwise = if even (countOp b) then (Just PlayerTrue) else (Just PlayerFalse)

actions :: ContingencyGame -> ContingencyPlayer -> [ContingencyAction]
actions g@(Board b _ str) p
  | activePlayer g /= Just p = error "!!!"
  | otherwise = let empty = emptyPositions b
                    adj = (fullPositions b) ++ [(x,y) | x<-[-1,7], y<-[0..6]] ++ [(x,y) | x<-[0..6], y<-[-1,7]] in
                      [Action str (x,y) Vertical | (x,y)<-emptyPositions b, elem (x-1,y) adj && elem (x+1,y) adj] ++
                      [Action str (x,y) Horizontal | (x,y)<-emptyPositions b, elem (x,y-1) adj && elem (x,y+1) adj]

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState g@(Board b bag s) p (Action str (x,y) d) =
  let i = toIndex (x,y) in
   if (b!!i) == Empty && (activePlayer g) == (Just p) then
     let newBoard = (flipPiece (insertAt b (toPiece str d) i) (adjacent (x,y))) in
       if length bag > 0 then
         do index <- randomRIO (0,length bag - 1)
            let newPiece = bag!!index
            return (Board newBoard (deleteAt bag index) newPiece)
       else return (Board newBoard bag "")
   else error "!!!"

isFinished :: ContingencyGame -> Bool
isFinished (Board b bag str) = (countOp b == 30) || (null bag && null str)

score :: ContingencyGame -> ContingencyPlayer -> Int
score game@(Board b _ _) player =
  let fun = (\p l -> if (p == PlayerTrue) then (length $ elemIndices True l)
                                          else (length $ elemIndices False l)) in
    (fun player) (map (getValue game) (fullPositions b))

showBoard :: ContingencyGame -> String
showBoard (Board b bag p) = (unlines $ map concat $ chunksOf 7 (map show b)) ++
                            (unwords bag) ++
                            ('\n':p) ++ "\n"

showAction :: ContingencyAction -> String
showAction (Action "iff" c Horizontal) = "i " ++ (show c)
showAction (Action "iff" c Vertical)   = "I " ++ (show c)
showAction (Action "xor" c Horizontal) = "x " ++ (show c)
showAction (Action "xor" c Vertical)   = "X " ++ (show c)
showAction (Action "and" c Horizontal) = "a " ++ (show c)
showAction (Action "and" c Vertical)   = "A " ++ (show c)
showAction (Action "or" c Horizontal)  = "o " ++ (show c)
showAction (Action "or" c Vertical)    = "O " ++ (show c)

readAction :: String -> ContingencyAction
readAction str
    | l!!0 == "i" = (Action "iff" c Horizontal)
    | l!!0 == "I" = (Action "iff" c Vertical)
    | l!!0 == "x" = (Action "xor" c Horizontal)
    | l!!0 == "X" = (Action "xor" c Vertical)
    | l!!0 == "a" = (Action "and" c Horizontal)
    | l!!0 == "A" = (Action "and" c Vertical)
    | l!!0 == "o" = (Action "or" c Horizontal)
    | l!!0 == "O" = (Action "or" c Vertical)
    | otherwise   = error $ show c
      where l = words str
            c = read (l!!1)::(Int,Int)

-- Match controller -------------------------------------------------------------------------------

type Agent = ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
consoleAgent,randomAgent,cheatyAgent,mcAgent :: Agent
consoleAgent g@(Board b bag s) player =
  do line <- getLine
     let (x,y) = fromLetters line
         actionList = actions g player
         aux = [act | act@(Action str (a,b) d)<-actionList, (a,b) == (x,y)]
     if null aux then do putStrLn "Well this is embarrassing..."
                         try <- (consoleAgent g player)
                         return try
     else return (aux!!0)

randomAgent g@(Board b bag s) player =
  do let actionList = actions g player
     i <- randomRIO (0,length actionList - 1)
     return (actionList!!i)

cheatyAgent g player =
  do let actionList = actions g player
     stateList <- sequence [nextState g player act | act <- actionList]
     let scoreList = [partialScore gx player | gx<-stateList]
         i = snd $ maximum $ zip scoreList [0..]
     return (actionList!!i)

mcAgent g player =
  do let actionList = actions g player
     stateList <- sequence [nextState g player act | act <- actionList]
     games <- sequence (map runGames stateList)
     let prob = map (\x -> (fromIntegral . length $ elemIndices True x) / (fromIntegral . length $ x)) games
     return (snd $ maximumBy (\x y -> compare (fst x) (fst y)) (zip prob actionList))
  where runGames game = do list <- sequence (take 10 (repeat (runRandom (PlayerTrue,PlayerFalse) game)))
                           let fun = if player == PlayerTrue then (\x -> (fst x) > (snd x))
                                                             else (\x -> (snd x) > (fst x))
                           return $ map fun list

type ContingencyMatch = (ContingencyPlayer, ContingencyPlayer) -> ContingencyGame -> IO (Int, Int)
runMatch :: ContingencyMatch
runMatch players@(agTrue, agFalse) g@(Board b bag str) =
  let active = fromJust (activePlayer g) in
  if (isFinished g) then
    do let newBoard = flipPiece b [(x,y) | x<-[0..6],y<-[0..6]]
           newGame = (Board newBoard bag str)
       putStrLn . show $ newGame
       return . scores $ newGame --(score newGame agTrue, score newGame agFalse)
  else do putStrLn (showBoard g);
          nextAction <- if active == PlayerTrue then (cheatyAgent g active) -- (consoleAgent g active)
                                                else (cheatyAgent g active)
          nextBoard <- (nextState g active nextAction);
          runMatch players nextBoard

runOnConsole :: IO (Int, Int)
runOnConsole = do
  board <- beginning
  runMatch (PlayerTrue, PlayerFalse) board

-- Funciones Auxiliares ---------------------------------------------------------------------------

randomBools :: [Bool] -> IO [Bool]
randomBools list = do l <- shuffle list
                      return (take 15 l)

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x
            else do i <- randomRIO(0,(length x)-1)
                    r <- shuffle (deleteAt x i)
                    return (x!!i : r)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = map (map snd) $ groupBy groupFunction (zip [0..] xs)
                where groupFunction (i1,_) (i2,_) = (div i1 n) == (div i2 n)

insertAt :: [a] -> a -> Int -> [a]
insertAt l e i = (take i l) ++ (e:(drop (i+1) l))

deleteAt :: [a] -> Int -> [a]
deleteAt l i = (take i l) ++ (drop (i+1) l)

countOp :: [Piece] -> Int
countOp b = sum $ map fun b
                    where fun Empty      = 0
                          fun (Hidden _) = 0
                          fun True_      = 0
                          fun False_     = 0
                          fun _          = 1

toPiece :: String -> Direction -> Piece
toPiece str d
  | str == "iff" = (OpIff d)
  | str == "xor" = (OpXor d)
  | str == "or"  = (OpOr d)
  | str == "and" = (OpAnd d)
  | otherwise = error "!!!"

toIndex :: (Int,Int) -> Int
toIndex (x,y) = 7*x+y

fromIndex :: Int -> (Int,Int)
fromIndex i = divMod i 7

fromLetters :: String -> (Int,Int)
fromLetters str
  | length str /= 2 = (-1,-1)
  | otherwise       = ((ord $ toLower (str!!0))-97,(ord (str!!1))-49)

emptyPositions,fullPositions :: [Piece] -> [(Int,Int)]
emptyPositions b  = [fromIndex i | i<-(elemIndices Empty b)]
fullPositions b   = [fromIndex i | (x,i)<-(zip b [0..]),x/=Empty]

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(a,b) | (a,b)<-[(x-1,y),(x+1,y),(x,y-1),(x,y+1)],a>=0,b>=0,a<7,b<7]

getValue :: ContingencyGame -> (Int,Int) -> Bool
getValue _ (-1,_) = True
getValue _ (_,-1) = True
getValue _ (7,_)  = False
getValue _ (_,7)  = False
getValue g@(Board b _ _) pos
  | b!!(toIndex pos) == True_  = True
  | b!!(toIndex pos) == False_ = False
getValue g pos = pieceValue pos g

pieceValue :: (Int,Int) -> ContingencyGame -> Bool
pieceValue p@(x,y) g@(Board b _ _)
  | (b!!(toIndex p) == (OpAnd Vertical))   = (getValue g (x-1,y)) && (getValue g (x+1,y))
  | (b!!(toIndex p) == (OpAnd Horizontal)) = (getValue g (x,y-1)) && (getValue g (x,y+1))
  | (b!!(toIndex p) == (OpOr Vertical))    = (getValue g (x-1,y)) || (getValue g (x+1,y))
  | (b!!(toIndex p) == (OpOr Horizontal))  = (getValue g (x,y-1)) || (getValue g (x,y+1))
  | (b!!(toIndex p) == (OpXor Vertical))   = xor (getValue g (x-1,y)) (getValue g (x+1,y))
  | (b!!(toIndex p) == (OpXor Horizontal)) = xor (getValue g (x,y-1)) (getValue g (x,y+1))
  | (b!!(toIndex p) == (OpIff Vertical))   = iff (getValue g (x-1,y)) (getValue g (x+1,y))
  | (b!!(toIndex p) == (OpIff Horizontal)) = iff (getValue g (x,y-1)) (getValue g (x,y+1))
  | otherwise = error $ show p

flipPiece :: [Piece] -> [(Int,Int)] -> [Piece]
flipPiece board [] = board
flipPiece board (pos:xs)
  | board!!i == (Hidden True) = flipPiece (insertAt board True_ i) xs
  | board!!i == (Hidden False) = flipPiece (insertAt board False_ i) xs
  | otherwise = flipPiece board xs
    where i = toIndex pos

partialScore :: ContingencyGame -> ContingencyPlayer -> Int
partialScore game@(Board b _ _) player =
  let playerScore = (\x -> (sum (map (partialValue game x) (fullPositions b))))
      (p1,p2) = if player == PlayerTrue then (PlayerTrue,PlayerFalse)
                                        else (PlayerFalse,PlayerTrue) in
      (playerScore p1 - playerScore p2)

partialValue :: ContingencyGame -> ContingencyPlayer -> (Int,Int) -> Int
partialValue g@(Board b _ _) player pos
  | b!!(toIndex pos) == (Hidden True)  = 0
  | b!!(toIndex pos) == (Hidden False) = 0
  | b!!(toIndex pos) == Empty          = 0
  | otherwise                          = fun (getValue g pos)
    where fun = if player == PlayerTrue then (\x -> if x then 1 else 0)
                                        else (\x -> if x then 0 else 1)

scores :: ContingencyGame -> (Int,Int)
scores game@(Board b _ _) = let list = (map (getValue game) (fullPositions b)) in
                            ((length $ elemIndices True list),(length $ elemIndices False list))

runRandom :: ContingencyMatch
runRandom players@(agTrue, agFalse) g@(Board b bag str) =
  let active = fromJust (activePlayer g) in
  if (isFinished g) then
    do let newBoard = flipPiece b [(x,y) | x<-[0..6],y<-[0..6]]
           newGame = (Board newBoard bag str)
       return . scores $ newGame --(score newGame agTrue, score newGame agFalse)
  else do nextAction <- (randomAgent g active)
          nextBoard <- (nextState g active nextAction);
          runRandom players nextBoard
