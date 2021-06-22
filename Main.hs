import System.IO ()  
import Control.Monad ()
import Text.Printf ()

import Help ( help )
import Move
    ( Move(InvalidM),
      stringToDir,
      movesToString,
      stringsToMove,
      stringToMoveString )

import Board ( showBoard, startPos )

import Functions ( play )


import Solver
    ( checkSolvable,
      checkGame,
      coordToDirections,
      removeConsecutiveDuplicates )  

import Data.List ()
import System.Exit ()
import Control.Concurrent ()
import System.Timeout ()



--Main functionality of the program
mainbuilder :: [String] -> IO ()
mainbuilder b = do
	putStr ">"
	line <- getLine
	let command = words line
	--print command
	case command of
		["load",filename] -> do
			map <- readFile filename
			putStrLn "Read map successfully!"
			putStrLn "Initial:"
			let board = lines map
			showBoard board
			mainbuilder board
		["check"] -> do
			if length b == 0
				then do
					putStrLn "Cannot check before loading a map"
					mainbuilder b
			else do
				answer <- checkGame b
				if answer == "Unsolvable"
					then do
						putStrLn "This map is unsolvable"
						mainbuilder b
				else do
					putStrLn "This map can be solved"
					mainbuilder b
		["solve"] -> do
			if length b == 0
				then do
					putStrLn "Cannot solve before loading a map"
					mainbuilder b
			else do
				check <- checkGame b
				if check == "Solvable"
					then do
						path <- checkSolvable b (startPos b) [] "Null"
						let directions = coordToDirections b (reverse path)
						let answer = removeConsecutiveDuplicates directions
						let movestr = stringToMoveString answer
						let finalanswer = stringsToMove movestr
						print finalanswer
						mainbuilder b
				else do
					putStrLn "This map cannot be solved"
					mainbuilder b
		["solveinstring"] -> do
			if length b == 0
				then do
					putStrLn "Cannot solve before loading a map"
					mainbuilder b
			else do
				check <- checkGame b
				if check == "Solvable"
					then do
						path <- checkSolvable b (startPos b) [] "Null"
						let directions = coordToDirections b (reverse path)
						let answer = removeConsecutiveDuplicates directions
						let movestr = stringToMoveString answer
						let initialanswer = stringsToMove movestr
						let finalanswer = movesToString initialanswer
						print finalanswer
						mainbuilder b
				else do
					putStrLn "This map cannot be solved"
					mainbuilder b

		["play",dir1,dir2,dir3]-> do
			if length b == 0
				then do
					putStrLn "Cannot play before loading a map"
					mainbuilder b
			else do
				if dir1 == "Left" || dir1 == "Right" || dir1 == "Up" || dir1 == "Down" &&
				   dir2 == "Left" || dir2 == "Right" || dir2 == "Up" || dir2 == "Down" &&
				   dir3 == "Left" || dir3 == "Right" || dir3 == "Up" || dir3 == "Down" 
				   then do 
					   d1 <- stringToDir dir1
					   d2 <- stringToDir dir2
					   d3 <- stringToDir dir3
					   play b d1 d2 d3
					   mainbuilder b
					else do
					   putStrLn "Invalid play pattern"
					   mainbuilder b
		
		["play"]-> do
			if length b == 0
				then do
					putStrLn "Cannot play before loading a map"
					mainbuilder b
			else do 
					play b InvalidM InvalidM InvalidM
					mainbuilder b
		["help"] -> do
			help
			mainbuilder b
		
		["hint"] -> do
			check <- checkGame b
			if check=="Solvable"
				then do
					path <- checkSolvable b (startPos b) [] "Null"
					let directions = coordToDirections b (reverse path)
					let answer = removeConsecutiveDuplicates directions 
					let h = head answer
					let message = "The first command to solve the game is "++h
					putStrLn message
					mainbuilder b
			else do
				putStrLn "This map is not solvable"
				mainbuilder b
				
		["showboard"] -> do
			if length b == 0
				then do
					putStrLn "Please laod a map before using showboard"
					mainbuilder b
			else do
				putStrLn "The board is-" 
				showBoard b
				mainbuilder b

		["quit"] -> do
			return ()
		_ -> do
			putStrLn "Invalid Command"
			mainbuilder b
		



--Main Function to start the game
main :: IO ()
main = do
	mainbuilder []	
 
