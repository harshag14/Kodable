module Help where

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

-- Help function to interact with the users
help :: IO ()
help = do
	putStrLn "1-How to play"
	putStrLn "2-Commands available"
	putStrLn "3-Input Format for play function"
	putStrLn "4-Back to main"
	putStrLn ""
	putStr ">>"
	userinput <- getLine 
	case userinput of
		['1'] -> do
			putStrLn "KODABLE GAME INSTRUCTIONS-"
			putStrLn ""
			putStrLn "@ represents the ball"
			putStrLn "- represents a path block that the ball could roll on"
			putStrLn "* represents the obstacles"
			putStrLn "p, y and o represents the tile for conditional statements"
			putStrLn "b represents the bonus"
			putStrLn "t represents the target point"
			putStrLn ""
			putStrLn "1) The ball will from the start point initially"
			putStrLn "2) The ball needs to roll from the start point to the end point. And it should find the best path to get all the three bonus points"
			putStrLn "3) Once it has decided the direction, the ball will stop only when it comes across obstacles or conditional blocks"
			putStrLn "4) If the ball comes across a conditional tile then the ball will turn to the direction specified in the conditional instruction"
			putStrLn "5) The player can use functions, and functions should include only three directions"
			putStrLn ""
			putStrLn "Your aim is to win the game by entering commands (Left,Right ...) sequentially"
			putStrLn "Check the pattern for entering commands through help"
			putStrLn ""
			help
		['2'] -> do
			putStrLn "COMMANDS AVAILABLE-"
			putStrLn ""
			putStrLn "1) 'load filename' to load a file and start the game"
			putStrLn "2) 'check' to cehck if a map is solvable"
			putStrLn "3) 'play' to play the game"
			putStrLn "4) 'solve' to get the solution"
			putStrLn "5) 'help' to get instructions about the game"
			putStrLn "6) 'showboard' to show the initial board"
			putStrLn "7) 'solveinstring' to show solution of the board in string format which is easier to read"
			putStrLn "8) 'hint' to get the first command to solve the game"
			putStrLn "9) 'quit' to quit the game"
			putStrLn ""
			help

		['3'] -> do
			putStrLn "INPUT PATTERN-"
			putStrLn ""
			putStrLn "Basic Commands:"
			putStrLn "Right -> To move in the right direction"
			putStrLn "Left -> To move in the left direction"
			putStrLn "Up -> To move in the upwards direction"
			putStrLn "Down -> To move in the downwards direction"
			putStrLn ""
			putStrLn "Additional Commands:"
			putStrLn "Conditions -> You can use conditionals by typing someting like 'Cond p direction' where direction is any of the basic commands "
			putStrLn "Function -> Enter Function while entering directions in play to use the three directions defined while useing play (Note- To use functions, you must start play with exactly 3 directions initially)"
			putStrLn "Loop -> To use loops to solve the maze. You can use use loops by typing something like 'Loop n direction direction' where n<=5 and direction can be any basic command or conditional"
			putStrLn ""
			help
		
		['4'] -> do
			return ()
		_ -> do
			putStrLn "Please enter 1-4 only-"
			putStrLn ""
			help

            