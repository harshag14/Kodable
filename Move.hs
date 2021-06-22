module Move where

import Control.Applicative ()
import Control.Monad ( Monad(return) )
import Control.Monad.State ( Monad(return) )
import Prelude

import Data.Char ()
import Data.Functor ()
import Data.List ()
import Data.Monoid ()
import Data.Maybe ()

import Text.Printf ()

data Move = L | R | U | D | Cond Char Move | Loop Int Move Move | Func Move Move Move | InvalidM deriving (Show,Eq)

--Helps in parsing the 4 basic directions when entered by the user and converts them to the Move type
--This is a helper function for getiing the user inputs. Function implemented for modularity
stringToDir :: String -> IO Move
stringToDir "Left" = return L
stringToDir "Right" = return R
stringToDir "Down" = return D
stringToDir "Up" = return U
stringToDir _ = return InvalidM


--Helps in parsing the conditional case when entered by the user and converts it to the Move type
--This is a helper function for getiing the user inputs. Function implemented for modularity
stringToCond :: String  -> String -> IO Move
stringToCond variable direction = do
			let d = case direction of 
					"Right" -> R
					"Left" -> L
					"Up" -> U
					"Down" -> D
					otherwise -> InvalidM
			    ch = if variable `elem` ["p","y","o"]
						then head variable
						else 'E'
			if ch=='E' || d == InvalidM
				then return InvalidM
				else return (Cond ch d)


--Simplies the list of moves entered by the user to a list with only L,R,U,D and Cond types
listOfMoves :: [Move] -> [Move]
listOfMoves [] = []
listOfMoves (Func a b c:xs) = listOfMoves [a,b,c] ++ listOfMoves xs
listOfMoves (Cond ch dir:xs) = Cond ch dir : listOfMoves xs
listOfMoves (Loop 0 a b:xs) = listOfMoves xs
listOfMoves (Loop n a b:xs) = listOfMoves [a]++listOfMoves [b]++listOfMoves (Loop (n-1) a b:xs)
listOfMoves (L:xs)= L : listOfMoves xs
listOfMoves (R:xs)= R : listOfMoves xs
listOfMoves (U:xs)= U : listOfMoves xs
listOfMoves (D:xs)= D : listOfMoves xs



--Converts a move to String
moveToString :: Move -> String 
moveToString L = "Left"
moveToString R = "Right"
moveToString U = "Up"
moveToString D = "Down"
moveToString (Cond ch d) = "Cond "++[ch,' ']++(moveToString d)

--Converts a list of moves to a list of Strings
movesToString :: [Move] -> [String]
movesToString [] = []
movesToString (x:xs) = moveToString x:movesToString xs

--Converts a string to a move
stringToMove :: String -> Move
stringToMove "Right" = R
stringToMove "Left" = L
stringToMove "Up" = U
stringToMove "Down" = D
stringToMove "Cond p Right" = Cond 'p' R
stringToMove "Cond p Left" = Cond 'p' L
stringToMove "Cond p Up" = Cond 'p' U
stringToMove "Cond p Down" = Cond 'p' D
stringToMove "Cond y Right" = Cond 'y' R
stringToMove "Cond y Left" = Cond 'y' L
stringToMove "Cond y Up" = Cond 'y' U
stringToMove "Cond y Down" = Cond 'y' D
stringToMove "Cond o Right" = Cond 'o' R
stringToMove "Cond o Left" = Cond 'o' L
stringToMove "Cond o Up" = Cond 'o' U
stringToMove "Cond o Down" = Cond 'o' D


--Converts a list of strings to a list of moves
stringsToMove :: [String] -> [Move]
stringsToMove [] = []
stringsToMove (x:xs) = stringToMove x:stringsToMove xs 


stringToMoveString :: [String] -> [String]
stringToMoveString [] =[]
stringToMoveString [x] = [x]
stringToMoveString [x,y] = [x,y]
stringToMoveString (x:xs:xss:xsss)
  								| x == "Left" = "Left":stringToMoveString (xs:xss:xsss)
								| x == "Right" = "Right":stringToMoveString (xs:xss:xsss)
  								| x == "Up" = "Up":stringToMoveString (xs:xss:xsss)
  								| x == "Down" = "Down":stringToMoveString (xs:xss:xsss)
  								| x == "Cond" = (x++" "++xs++" "++xss):stringToMoveString (xsss)
																  
