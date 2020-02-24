{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.List.Split
import Data.List
import Data.Set as Set

data Symbol = X | O deriving (Show,Eq)
data Position = Zero | One | Two | Three | Four | Five | Six | Seven | Eight  deriving(Show,Eq,Enum,Ord)
data Player = Player {name :: String, symbol :: Symbol} deriving (Show,Eq)
data GameState = InPlay Player Player Board | HasWon Player | Drawn deriving(Show,Eq)

type Board = (Set Position, Set Position)

diagonalComb = [[Zero,Four,Eight], [Two,Four,Six]]
rowComb = chunksOf 3 [Zero .. Eight]
columnComb = transpose rowComb
winComb = Set.fromList (Prelude.map Set.fromList (rowComb ++ columnComb ++ diagonalComb))

hasWon :: Set Position -> Bool
hasWon moves = any ((flip Set.isSubsetOf) moves) winComb

mapPosition :: String -> Maybe Position
mapPosition charCh
  | charCh `elem` (Prelude.map show [0..8]) = Just (choices !! (read charCh))
  | otherwise = Nothing
  where choices = [Zero .. Eight]

updateBoard :: Board -> Symbol -> Position -> Board
updateBoard (xs, os) sm pos
  | sm == X = (Set.insert pos xs, os)
  | sm == O = (xs, Set.insert pos os)
  | otherwise = (xs, os)

playGame :: GameState -> IO String
playGame (HasWon p) = return ((name p) ++ " has won")
playGame Drawn = return ("game is drawn")
playGame (InPlay pNow pNext (xs, os))
  |((hasWon xs) || (hasWon os)) == True = playGame (HasWon pNext)
  |(length xs + length os) == 9 = playGame Drawn
  |otherwise = (makeMove pNow (xs,os)) >>= continueGame pNow pNext (xs,os)

continueGame :: Player-> Player -> Board -> (Maybe Position) -> IO String
continueGame pNow pNext (xs,os) Nothing = do
  putStrLn ("Invalid move, Please choose from the given options")
  playGame (InPlay pNow pNext (xs, os))
continueGame pNow pNext (xs,os) (Just pos) = do
  if (Set.member pos (Set.union xs os))
  then do
   putStrLn ("Already occupied, please choose an available cell")
   playGame (InPlay pNow pNext (xs, os))
  else do
   let (xs',os') = updateBoard (xs, os) (symbol pNow) pos
   playGame (InPlay pNext pNow (xs',os'))


makeMove :: Player -> Board -> IO (Maybe Position)
makeMove p (xs,os) = do
  printBoard (xs,os)
  putStrLn ((name p) ++ ", enter your move :")
  choice <- getLine
  return (mapPosition choice)

startGame :: IO String
startGame = do
  putStrLn "Player 1 enter your name :"
  p1Name <- getLine
  putStrLn "Player 2 enter your name :"
  p2Name <- getLine
  playGame $ InPlay (Player p1Name X) (Player p2Name O) (Set.empty,Set.empty)

insertSymbol :: [Maybe Int] -> [Maybe Int] -> [String]
insertSymbol xPos oPos = do
  let xPosStr = Prelude.map (\(Just x) -> (show x)) xPos
  let oPosStr = Prelude.map (\(Just x) -> (show x)) oPos
  let xUpdatedData = Prelude.map (\(e) -> if (elem e xPosStr) then show(X) else e) (Prelude.map show [0..8])
  let fullUpdatedData = Prelude.map (\(e) -> if (elem e oPosStr) then show(O) else e) xUpdatedData
  fullUpdatedData

printBoard :: Board -> IO ()
printBoard (xs,os) =
 (let xPos = Data.List.map ((flip elemIndex) [Zero .. Eight]) (Set.toAscList xs)
      oPos = Data.List.map ((flip elemIndex) [Zero .. Eight]) (Set.toAscList os)
      updatedBoard = insertSymbol xPos oPos
      boardWithData = intercalate "\n- + - + -\n" (Prelude.map (intercalate " | ") (chunksOf 3 updatedBoard))
  in putStrLn (boardWithData))

--      boardWithData = intercalate "\n- + - + -\n" (Prelude.map (intercalate " | ") (chunksOf 3 square_values))

main :: IO ()
main = do
  print "-------------------Started-------------------"
  game <- startGame
  print game
