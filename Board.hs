module Board where

import Control.Applicative ()
import Control.Monad ( mapM_ )
import Control.Monad.State ( mapM_ )
import Prelude

import Data.Char ()
import Data.Functor ()
import Data.List ( elemIndices )
import Data.Monoid ()
import Data.Maybe ()

import Text.Printf ()



--To display the map
outputMap :: String -> IO ()
outputMap board = mapM_ putStrLn $ lines board

--2D list to Output function
showBoard :: [String] -> IO ()
showBoard = mapM_ putStrLn

--Takes board to find the starting line
findStartLine :: Foldable t => [t Char] -> t Char
findStartLine (x:xs) = if '@' `elem` x then x else findStartLine xs

--Takes board to find the position of @
findStartElement :: [String] -> [Int]
findStartElement board = elemIndices '@' $ findStartLine board

--Takes board to find the starting line position
findStartLinePos :: [[Char]] -> [Int]
findStartLinePos xs = elemIndices (findStartLine xs) xs

--Gets the position of the start '@' on board
startPos :: [String] -> [Int]
startPos board = (findStartLinePos board) !! 0 : (findStartElement board !! 0) : []

--Takes board to find the ending line
findEndLine :: Foldable t => [t Char] -> t Char
findEndLine (x:xs) = if 't' `elem` x then x else findEndLine xs

--Takes board to find the position of t
findEndElement :: [String] -> [Int]
findEndElement board = elemIndices 't' $ findEndLine board

--Takes board to find the ending line position
findEndLinePos :: [[Char]] -> [Int]
findEndLinePos xs = elemIndices (findEndLine xs) xs

--Gets the position of the target 't' on board
endPos :: [String] -> [Int]
endPos board = (findEndLinePos board) !! 0 : (findEndElement board !! 0) : []

--Returns the length of the board
boardLength :: Foldable t => [t a] -> [Int]
boardLength board = length board : length (board !! 0) : []

--Checks if a given index is within board range or not
checkValidPos :: [String] -> [Int] -> Bool
checkValidPos board xs = if (xs !! 0) >= 0 && (xs !! 0) < (boardLength board !! 0) && (xs !! 1) >=0 && (xs !! 1) < (boardLength board !! 1)  then True else False


-- 1 move right side on the board
moveRight :: Num a => [a] -> [a]
moveRight xs = xs!!0 : (xs!!1) + 2 : []  

-- 1 move left side on the board
moveLeft :: Num a => [a] -> [a]
moveLeft xs = xs!!0 : (xs!!1) - 2 : []  

-- 1 move upward on the board
moveUp :: Num a => [a] -> [a]
moveUp xs = (xs!!0) - 1 : xs!!1 : []  


-- 1 move downward on the board
moveDown :: Num a => [a] -> [a]
moveDown xs = (xs!!0) + 1 : xs!!1 : []  


--Get the character at (x,y) position from the board
getPos :: [String] -> [Int] -> Char
getPos board xs = (board !! (xs !! 0)) !! (xs !! 1) 


-- Move right N times until * or any special character is encountered
moveRightN :: [String] -> [Int] -> [Int]
moveRightN board xs = if getPos board xs == '-' then moveRightN board $ moveRight xs
                 else xs!!0 : (xs !! 1) - 2 : [] 


--Move left N times	until * or any special character is encountered
moveLeftN :: [String] -> [Int] -> [Int]
moveLeftN board xs = if getPos board xs == '-' then moveLeftN board $ moveLeft xs
				else xs!!0 : (xs !! 1) + 2 : []

--Move Up N times until * or any special character is encountered
moveUpN :: [String] -> [Int] -> [Int]
moveUpN board xs = if getPos board xs == '-' then moveUpN board $ moveUp xs
				else (xs!!0)+1 : xs !! 1 : []
	
--Move Down N times until * or any special character is encountered
moveDownN :: [String] -> [Int] -> [Int]
moveDownN board xs = if getPos board xs == '-' then moveDownN board $ moveDown xs
				else (xs!!0)-1 : xs !! 1 : []


-- Checks if the position is not equal to '*' and it is within the board
validMove :: [String] -> [Int] -> Bool
validMove board xs =if (checkValidPos board xs) == True && getPos board xs /= '*' then True else False

--Updates a the board by taking an index and character and replacing the character on the board at the given index with the given character
updateBoard :: [[a]] -> a -> [Int] -> [[a]]
updateBoard m x xs = take (xs!!0) m ++
  					[take (xs!!1) (m !! (xs!!0)) ++ [x] ++ drop ((xs!!1) + 1) (m !! (xs!!0))] ++
  					drop ((xs!!0) + 1) m




					  