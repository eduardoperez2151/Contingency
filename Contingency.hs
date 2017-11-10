{-
	Plantilla de c�digo para el proyecto del curso 2017 de "Electiva - Programaci�n Funcional" para
	las carreras de Ingenier�a y Licenciatura en Inform�tica de la FIT (UCU).

	Por Leonardo Val.
-}
module Contingency where

import Data.Maybe
import Data.List

-- Game logic stub ---------------------------------------------------------------------------------

data ContingencyPlayer = PlayerTrue | PlayerFalse deriving (Eq, Show, Enum)
data ContingencyGame   = Board [Piece] -- 7*7 = 49
data ContingencyAction = Move Operador Bool (Int,Int)

data Piece = Empty | Hidden Bool | True_ | False_ | OpAnd Bool
             | OpOr Bool | OpIff Bool | OpXor Bool
instance Show Piece where
  show Empty = "."
  show (Hidden _) = "0"
  show True_ = "t"
  show False_ = "f"
  show (OpAnd _) = "&"
  show (OpOr _) = "|"
  show (OpIff _) = "i"
  show (OpXor _) = "x"

type Operador = (Bool -> Bool -> Bool)
iff, xor :: Operador
iff p q = (p && q) || ((not p) && (not q))
xor p q = ((not p) && q) || (p && (not q))

beginning :: IO ContingencyGame
beginning = error "beginning has not been implemented!" --TODO

emptyBoard :: ContingencyGame
emptyBoard = Board (replicate 49 Empty)

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer g@(Board b)
  | isFinished g = Nothing
  | otherwise = if even (countOp b) then (Just PlayerTrue) else (Just PlayerFalse)

actions :: ContingencyGame -> ContingencyPlayer -> [ContingencyAction]
actions _ _ = error "actions has not been implemented!" --TODO

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState _ _ _ = error "nextState has not been implemented!" --TODO

isFinished :: ContingencyGame -> Bool
isFinished (Board b) = countOp b == 30

score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "score has not been implemented!" --TODO

showBoard :: ContingencyGame -> String
showBoard (Board b) = unlines $ map concat $ chunksOf 7 (map show b)

showAction :: ContingencyAction -> String
showAction _ = error "showAction has not been implemented!" --TODO

readAction :: String -> ContingencyAction
readAction _ = error "readAction has not been implemented!" --TODO

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
{-chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []
-}
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
