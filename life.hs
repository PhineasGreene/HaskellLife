import Data.List

-- takes the x and y position of a cell and finds the state of that cell in the grid
findStateAt :: (Int, Int) -> [(Int, Int, Int)] -> Int
findStateAt (x, y) grid
       | (find(==(x, y, 1)) grid) /= Nothing = 1
       | otherwise = 0

-- generates a grid of cells. a one dimentional list of cells with x and y positions as well as states. cells are set to state 0 unless there is a cell at that position with state 1 in the list "cells" passed by the user 
genGrid :: Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
genGrid x y width height cells
       | (y <= height) && (x < width) = (x, y, state) : genGrid (x+1) y width height cells
       | (y < height) && (x >= width) = (x, y, state) : genGrid 0 (y+1) width height cells
       | otherwise = (x, y, 0) : []
       where state = findStateAt (x, y) cells

-- used for printing the grid.
showState :: (Int, Int, Int) -> String
showState c
       | (getState c) == 0 = "."
       | otherwise = "O"

-- get cell info
getState :: (Int, Int, Int) -> Int
getState (a, b, c) = c

getY :: (Int, Int, Int) -> Int
getY (a, b, c) = b

getX :: (Int, Int, Int) -> Int
getX (a, b, c) = a

-- takes a list of cells and returns a string that can be printed to the console
strGrid :: [(Int, Int, Int)] -> Int -> Int -> String 
strGrid a i y
       | (i < l) && (cellY == y) = state ++ strGrid a (i+1) y
       | (i < l) && (cellY /= y) = "\n" ++ state ++ strGrid a (i+1) (y+1)
       | otherwise = "\n"
       where state = showState (a !! i)
             cellY = getY (a !! i)
             l = length a

-- count any cells living neighbors. due to the nature of findStateAt, off grid neighbors are assumed to be of state 0
countLiving :: (Int, Int, Int) -> [(Int, Int, Int)] -> Int
countLiving cell grid = (findStateAt (x - 1, y - 1) grid) +
                        (findStateAt (x, y - 1) grid) +
                        (findStateAt (x + 1, y - 1) grid) +
                        (findStateAt (x - 1, y) grid) +
                        (findStateAt (x + 1, y) grid) +
                        (findStateAt (x - 1, y + 1) grid) +
                        (findStateAt (x, y + 1) grid) +
                        (findStateAt (x + 1, y + 1) grid)
       where x = getX cell
             y = getY cell

-- do the actual Game of Life stuff
updateGrid :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
updateGrid old i
       | i >= (length old) = []
       | living == 2 = (x, y, state) : (updateGrid old (i+1))
       | living == 3 = (x, y, 1) : (updateGrid old (i+1))
       | otherwise = (x, y, 0) : (updateGrid old (i+1))
       where cell = old !! i
             living = countLiving cell old
             x = getX cell
             y = getY cell
             state = findStateAt (x, y) old

-- the grid we use for the life function
myGrid = genGrid 0 0 15 15 [(1, 2, 1), (2, 2, 1), (3, 2, 1), (7, 7, 1), (6, 7, 1), (8, 7, 1), (7, 6, 1), (7, 8, 1), (2, 1, 1), (2, 3, 1)]

-- run it in a little UI
life grid = do
       putStr $ strGrid (updateGrid grid 0) 0 0
       tmp <- getLine
       life $ updateGrid grid 0
