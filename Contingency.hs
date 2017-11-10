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

data Piece = Empty | Hidden Bool | True_ | False_ | Op Operador Bool

type Operador = (Bool -> Bool -> Bool)
iff, xor :: Operador
iff p q = (p && q) || ((not p) && (not q))
xor p q = ((not p) && q) || (p && (not q))

beginning :: IO ContingencyGame
beginning = error "beginning has not been implemented!" --TODO

emptyBoard :: ContingencyGame
emptyBoard = Board (replicate 74 Empty)

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer g@(Board b)
  | isFinished g = Nothing
  | otherwise = if even $ length [x | Op x _ <- b] then (Just PlayerTrue) else (Just PlayerFalse)

actions :: ContingencyGame -> ContingencyPlayer -> [ContingencyAction]
actions _ _ = error "actions has not been implemented!" --TODO

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState _ _ _ = error "nextState has not been implemented!" --TODO

isFinished :: ContingencyGame -> Bool
isFinished (Board b) = length [x | Op x _ <- b] == 30

score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "score has not been implemented!" --TODO

showBoard :: ContingencyGame -> String
showBoard _ = error "showBoard has not been implemented!" --TODO

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
