module Solver where

import Board
    ( startPos,
      endPos,
      moveRight,
      moveLeft,
      moveUp,
      moveDown,
      getPos,
      validMove )

    
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
import System.Timeout ( timeout )


--Helper funtion for coordToDirections
directionChange :: (Eq a, Num a) => [a] -> [a] -> [Char]
directionChange x y 
				|	(x!!0-y!!0) == 0 && (x!!1-y!!1) == -2 = "Right"
				|	(x!!0-y!!0) == 0 && (x!!1-y!!1) ==  2 = "Left"
				|	(x!!0-y!!0) == -1 && (x!!1-y!!1) ==  0 = "Down"
				|	(x!!0-y!!0) == 1 && (x!!1-y!!1) ==  0 = "Up"
				| otherwise = ""


--Changing the coordinates to directions for the solve function
coordToDirections :: [String] -> [[Int]] -> [[Char]]
coordToDirections board [] = []
coordToDirections board [x] = []
coordToDirections board (x:xs:xss) 
						| (getPos board x) /= 'p' && (getPos board x) /='y' && (getPos board x) /='o' = (directionChange x xs) : coordToDirections board (xs:xss)
						| (getPos board x) == 'p' = "Cond":"p":(directionChange x xs) : coordToDirections board (xs:xss)
						| (getPos board x) == 'y' = "Cond":"y":(directionChange x xs) : coordToDirections board (xs:xss)
						| (getPos board x) == 'o' = "Cond":"o":(directionChange x xs) : coordToDirections board (xs:xss)
						| otherwise = []


--Removes all the consecutive same directions
removeConsecutiveDuplicates :: Eq a => [a] -> [a]
removeConsecutiveDuplicates [] = []
removeConsecutiveDuplicates [x] = [x]
removeConsecutiveDuplicates (x:xs:xss) 
								| x == xs = removeConsecutiveDuplicates (xs:xss)
								| x /= xs = x  : removeConsecutiveDuplicates (xs:xss)
								| otherwise = []


-- Function to check if a given map is solvable or not
checkSolvable :: [String] -> [Int] -> [[Int]] -> String -> IO [[Int]]
checkSolvable board xs fin opp = do
				-- "Checking"
				let f = xs : fin
				--print xs
				if xs /= endPos board
					then do 
						--print f
						let positionRight = (moveRight xs)
						let positionLeft = (moveLeft xs)
						let positionUp = (moveUp xs)
						let positionDown = (moveDown xs)
						if validMove board positionRight && opp /= "Right" && (positionRight `elem` fin) /= True
							then do
								let op = "Left"
								p1 <- checkSolvable board positionRight f op
								if p1 == []
									then
										if validMove board positionLeft && opp /= "Left" && (positionLeft `elem` fin) /= True
											then do
												let op = "Right"
												p2 <- checkSolvable board positionLeft f op
												if p2 == []
													then do
														if validMove board positionUp && opp /= "Up" && (positionUp `elem` fin) /= True
															then do
																let op = "Down"
																p3 <- checkSolvable board positionUp f op
																if p3 == []
																	then
																		if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
																			then do
																				let op = "Up"
																				checkSolvable board positionDown f op
																			else do
																				return []
																	else do
																		return p3
															else do
																if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
																	then do
																		let op = "Up"
																		checkSolvable board positionDown f op
																	else do
																		return []
													else do
														return p2
											else do
												if validMove board positionUp && opp /= "Up" && (positionUp `elem` fin) /= True
													then do
														let op = "Down"
														p3 <- checkSolvable board positionUp f op
														if p3 == []
															then do
																if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
																	then do
																		let op = "Up"
																		checkSolvable board positionDown f op
																	else do
																		return []
															else do
																return p3
													else do
														if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
															then do
																let op = "Up"
																checkSolvable board positionDown f op
															else do
																return []
									else do
										return p1
							else do
								if validMove board positionLeft && opp /= "Left" && (positionLeft `elem` fin) /= True
									then do
										let op = "Right"
										p2 <- checkSolvable board positionLeft f op
										if p2 == []
											then do
												if validMove board positionUp && opp /= "Up" && (positionUp `elem` fin) /= True
													then do
														let op = "Down"
														p3 <- checkSolvable board positionUp f op
														if p3 == []
															then do
																if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
																	then do
																		let op = "Up"
																		checkSolvable board positionDown f op
																	else do
																		return []
															else do
																return p3
													else do
														if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
															then do
																let op = "Up"
																checkSolvable board positionDown f op
															else do
																return []
											else do
												return p2
									else do
										if validMove board positionUp && opp /= "Up" && (positionUp `elem` fin) /= True
											then do
												let op = "Down"
												p3 <- checkSolvable board positionUp f op
												if p3 == []
													then do
														if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
															then do
																let op = "Up"
																checkSolvable board positionDown f op
															else do
																return []
													else do
														return p3
											else do
												if validMove board positionDown && opp /= "Down" && (positionDown `elem` fin) /= True
													then do
														let op = "Up"
														checkSolvable board positionDown f op
													else do
														return []
						
				else do
					-- print f
					return f
					-- exitSuccess


--This function is a helper function for the check function
test :: [String] -> IO [[Int]]
test board = do
	f <- checkSolvable board (startPos board) [] "Null"
	return f

--Function for implementing the check functionality
checkGame :: [String] -> IO [Char]
checkGame board = do
		answer <- timeout 1000000 (test board)
		case answer of 
			Nothing -> do
					return "Unsolvable"
			Just _ -> do
					return "Solvable"



