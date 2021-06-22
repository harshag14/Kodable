module Functions where

import Help ()
import Board
    ( showBoard,
      startPos,
      endPos,
      checkValidPos,
      moveRight,
      moveLeft,
      moveUp,
      moveDown,
      getPos,
      updateBoard )

import Move ( Move(..), stringToDir, stringToCond, listOfMoves )

import Solver ()

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

--Gets input of the moves from the user and maps them to the Move datatype created above
--Shows an error message of there is a type error in the user input
getUserInputs :: Move -> Move -> Move -> IO [Move]
getUserInputs func1 func2 func3 = do
	putStr "Enter Direction: "
	line <- getLine
	let move = words line
	case move of
		["Right"] -> do
			moves <- getUserInputs func1 func2 func3 
			return (R:moves)
		["Left"] -> do
			moves <- getUserInputs func1 func2 func3
			return (L:moves)
		["Up"] -> do
			moves <- getUserInputs func1 func2 func3
			return (U:moves)
		["Down"] -> do
			moves <- getUserInputs func1 func2 func3
			return (D:moves)
		["Cond",variable,direction] -> do
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
				then do
					putStrLn "Error! Invalid Conditional Format"
					putStrLn "The conditional format is: 'Cond ch direction' where ch is 'p', 'o' or 'y'"
					getUserInputs func1 func2 func3
				else do
					moves <- getUserInputs func1 func2 func3
					return (Cond ch d:moves)
									
		["Function"] -> do
			if func1 == InvalidM || func2 == InvalidM || func3 == InvalidM
				then do
					putStrLn "Function not specified with the play command"
					getUserInputs func1 func2 func3
			else do
				moves <- getUserInputs func1 func2 func3
				return (Func func1 func2 func3:moves)

		["Loop",count,dir1,dir2] -> do
			let countint = read count :: Int
			let d1 = case dir1 of 
					"Right" -> R
					"Left" -> L
					"Up" -> U
					"Down" -> D
					otherwise -> InvalidM
			
			let d2 = case dir2 of 
					"Right" -> R
					"Left" -> L
					"Up" -> U
					"Down" -> D
					otherwise -> InvalidM
			
			if d1==InvalidM || d2 == InvalidM || countint < 0 || countint >5
				then do
					putStrLn "Error! Invalid Loop format"
					getUserInputs func1 func2 func3
				else do
					moves <- getUserInputs func1 func2 func3
					return (Loop countint d1 d2:moves)

			
		["Loop",count,"Cond",ch,dir1,dir2] -> do
			let countint = read count :: Int
			d1 <- stringToCond ch dir1
			d2 <- stringToDir dir2
			if d1==InvalidM || d2 == InvalidM || countint < 0 || countint >5
				then do
					putStrLn "Error! Invalid Loop format"
					getUserInputs func1 func2 func3
				else do
					moves <- getUserInputs func1 func2 func3
					return (Loop countint d1 d2:moves)
		
		["Loop",count,dir1,"Cond",ch,dir2] -> do
			let countint = read count :: Int
			d1 <- stringToDir dir1
			d2 <- stringToCond ch dir2
			if d1==InvalidM || d2 == InvalidM || countint < 0 || countint >5
				then do
					putStrLn "Error! Invalid Loop format"
					getUserInputs func1 func2 func3
				else do
					moves <- getUserInputs func1 func2 func3
					return (Loop countint d1 d2:moves)
		
		["Loop",count,"Cond",ch1,dir1,"Cond",ch2,dir2] -> do
			let countint = read count :: Int
			d1 <- stringToCond ch1 dir1
			d2 <- stringToCond ch2 dir2
			if d1==InvalidM || d2 == InvalidM || countint < 0 || countint >5
				then do
					putStrLn "Error! Invalid Loop format"
					getUserInputs func1 func2 func3
				else do
					moves <- getUserInputs func1 func2 func3
					return (Loop countint d1 d2:moves)

		[] -> return []
		otherwise -> do
			putStrLn "Error! Invalid Input Format! Please use help to check the input formats"
			getUserInputs func1 func2 func3 



--Applies moves entered by the user on the board.
--In the end it gives the final coordinate which could be achieved after applying all the moves
--Also gives an error with the list in case the input moves are not valid
applyMoves :: [String] -> [Int] -> Bool  -> [Move] -> [[Int]] -> IO [Int]
applyMoves board startposition counter [] bs = return startposition
applyMoves board startposition counter (move:moves) bs = do
					case move of
						R -> do
							let rightIndex = moveRight startposition
							if checkValidPos board rightIndex == False && counter == False
								then do return (startposition!!0:startposition!!1:0:[])
							else if checkValidPos board rightIndex == True && getPos board rightIndex == '*' && counter == False
								then do return (startposition!!0:startposition!!1:0:[])
							else if checkValidPos board rightIndex == True 
								then do
									let currentcounter = True
									if getPos board rightIndex == '-'
										then do applyMoves board rightIndex currentcounter (move:moves) bs
									else if getPos board rightIndex == 'b'
										then do
											if (rightIndex `elem` bs) == False 
												then do
													let bonus = rightIndex:bs
													let l = length bonus
													let message = "Bonus Number "++show l++" Collected"
													putStrLn message
													applyMoves board rightIndex currentcounter (move:moves) bonus
												else do
													applyMoves board rightIndex currentcounter (move:moves) bs
									else if getPos board rightIndex == 'p' || getPos board rightIndex == 'y' || getPos board rightIndex == 'o'
										then do
											if head moves == L || head moves == R || head moves == U || head moves == D
												then do
													--putStrLn "Invalid Moves"
													return []
											else do 
												applyMoves board rightIndex False moves bs
									else if getPos board rightIndex == 't'
										then do return rightIndex
									else do applyMoves board startposition False moves bs
							else do applyMoves board startposition False moves bs
						U -> do
							let upIndex = moveUp startposition
							if checkValidPos board upIndex == False && counter == False
								then do return (startposition!!0:startposition!!1:2:[])
							else if checkValidPos board upIndex == True && getPos board upIndex == '*' && counter == False
								then do return (startposition!!0:startposition!!1:2:[])
							else if checkValidPos board upIndex == True 
								then do
									let currentcounter = True
									if getPos board upIndex == '-'
										then do applyMoves board upIndex currentcounter (move:moves) bs
									else if getPos board upIndex == 'b'
										then do
											if (upIndex `elem` bs) == False 
												then do
													let bonus = upIndex:bs
													let l = length bonus
													let message = "Bonus Number "++show l++" Collected"
													putStrLn message
													applyMoves board upIndex currentcounter (move:moves) bonus
												else do
													applyMoves board upIndex currentcounter (move:moves) bs
									else if getPos board upIndex == 'p' || getPos board upIndex == 'y' || getPos board upIndex == 'o'
										then do
											if head moves == L || head moves == R || head moves == U || head moves == D
												then do
													--putStrLn "Invalid Moves"
													return []
											else do 
												applyMoves board upIndex False moves bs
									else if getPos board upIndex == 't'
										then do return upIndex
									else do applyMoves board startposition False moves bs
							else do
								applyMoves board startposition False moves bs
						L -> do
							let leftIndex = moveLeft startposition
							if checkValidPos board leftIndex == False && counter == False
								then do return (startposition!!0:startposition!!1:1:[])
							else if checkValidPos board leftIndex == True && getPos board leftIndex == '*' && counter == False
								then do return (startposition!!0:startposition!!1:1:[])
							else if checkValidPos board leftIndex == True 
								then do
									let currentcounter = True
									if getPos board leftIndex == '-'
										then do applyMoves board leftIndex currentcounter (move:moves) bs
									else if getPos board leftIndex == 'b'
										then do
											if (leftIndex `elem` bs) == False 
												then do
													let bonus = leftIndex:bs
													let l = length bonus
													let message = "Bonus Number "++show l++" Collected"
													putStrLn message
													applyMoves board leftIndex currentcounter (move:moves) bonus
												else do
													applyMoves board leftIndex currentcounter (move:moves) bs
									else if getPos board leftIndex == 'p' || getPos board leftIndex == 'y' || getPos board leftIndex == 'o'
										then do
											if head moves == L || head moves == R || head moves == U || head moves == D
												then do
													--putStrLn "Invalid Moves"
													return []
											else do 
												applyMoves board leftIndex False moves bs
									else if getPos board leftIndex == 't'
										then do return leftIndex
									else do applyMoves board startposition False moves bs
							else do applyMoves board startposition False moves bs
						D -> do
							let downIndex = moveDown startposition
							if checkValidPos board downIndex == False && counter == False
								then do return (startposition!!0:startposition!!1:3:[])
							else if checkValidPos board downIndex == True && getPos board downIndex == '*' && counter == False
								then do return (startposition!!0:startposition!!1:3:[])
							else if checkValidPos board downIndex == True 
								then do
									let currentcounter = True
									if getPos board downIndex == '-'
										then do applyMoves board downIndex currentcounter (move:moves) bs
									else if getPos board downIndex == 'b'
										then do
											if (downIndex `elem` bs) == False 
												then do
													let bonus = downIndex:bs
													let l = length bonus
													let message = "Bonus Number "++show l++" Collected"
													putStrLn message
													applyMoves board downIndex currentcounter (move:moves) bonus
												else do
													applyMoves board downIndex currentcounter (move:moves) bs
									else if getPos board downIndex == 'p' || getPos board downIndex == 'y' || getPos board downIndex == 'o'
										then do
											if head moves == L || head moves == R || head moves == U || head moves == D
												then do
													--putStrLn "Invalid Moves"
													return []
											else do 
												applyMoves board downIndex False moves bs
									else if getPos board downIndex == 't'
										then do return downIndex
									else do applyMoves board startposition False moves bs
							else do applyMoves board startposition False moves bs
						Cond ch dir -> do
							let currIndex = getPos board startposition
							if currIndex /= ch
								then do
									--putStrLn "Invalid Moves"
									return []
							else do
								applyMoves board startposition False (dir:moves) bs 
						_ -> do
							return []

--Modular function to get the error message from the error code returned by the applyMoves function							
getErrorMessage :: (Eq a, Num a) => a -> [Char]
getErrorMessage n 
			| n ==0 = "Sorry, error: cannot move to the right"
			| n ==1 = "Sorry, error: cannot move to the left"
			| n ==2 = "Sorry, error: cannot move up"
			| n ==3 = "Sorry, error: cannot move down"
			| otherwise = ""
            

--A function to implement the play functionality
--It users the getUserInputs and applyMoves functions created above and displays a board in the end
play :: [String] -> Move -> Move -> Move -> IO ()
play board dir1 dir2 dir3 = do
					inputs <- getUserInputs dir1 dir2 dir3 
					let simplifiedInputs = listOfMoves inputs
					endcoordinates <- applyMoves board (startPos board) False simplifiedInputs []
					let atposition = startPos board
					if length endcoordinates == 3
						then do
							let lastvalidposition = take 2 endcoordinates
							let errornumber = endcoordinates !! 2
							putStrLn (getErrorMessage errornumber)
							putStrLn "Board before it encountered error:"
							let newboard1 = updateBoard board '-' atposition
							let finalboard = updateBoard newboard1 '@' lastvalidposition
							showBoard finalboard
							return ()
					else if length endcoordinates == 2
						then do
							let winpos = endPos board
							if endcoordinates == winpos
								then do
									putStrLn "Final Board:"
									let newboard1 = updateBoard board '-' atposition
									let finalboard = updateBoard newboard1 '@' winpos
									showBoard finalboard
									putStrLn "Congratulations! You win the game!"
									return ()
							else do
								putStrLn "Final Board:"
								let lastvalidposition = take 2 endcoordinates
								let newboard1 = updateBoard board '-' atposition
								let finalboard = updateBoard newboard1 '@' lastvalidposition
								showBoard finalboard
								putStrLn "Sorry! You Lost!"
								return ()
					else do
						putStrLn "Error: Conditional not found in commands where it was expected"
						return ()

