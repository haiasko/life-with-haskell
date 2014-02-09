-- | Main entry point to the application.
module Main where

import Data.List (union,intersect,(\\),nub)

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Conway's Life"
    loop glider
    
loop :: World -> IO ()
loop w = do
    putStrLn $ showW w
    getLine
    loop $ stepW w
    
-- simplest impl - a list of live cells
type Cell = (Int, Int)
type World = [Cell]

glider = [(0,0), (1,0), (2,0), (0,1), (1,2)]

showCell :: World -> Cell -> Char 
showCell w c
    | c `elem` w = 'O'
    | c `elem` neighb = '_'
    | otherwise = '.'
    where
        neighb = neighborhood w

-- shows the whole world (min to max)
showW :: World -> String
showW w =
    show (minX, minY) ++ " " ++ show (maxX, maxY) ++ "\n" ++ 
         concat [showLine w y | y <- [minY..maxY]]
    where
        showLine w l = [showCell w (x, l) | x <- [minX..maxX]] ++ "\n"
        minX = (minimum $ map fst w) - 2 -- padding 2
        minY = (minimum $ map snd w) - 2
        maxX = (maximum $ map fst w) + 2
        maxY = (maximum $ map snd w) + 2

-- return the strict (nonlive) neighbours of live cells
neighborhood :: World -> World
neighborhood w =   (concat $ map cellNeighbors w) \\ w

-- not strict at all
cellNeighbors :: Cell -> World
cellNeighbors (x, y) = [(x + xx, y + yy) | xx <- [-1..1], yy <- [-1..1], (xx, yy) /= (0, 0)] -- sorry for the lazy clumsiness :)

-- step a world
stepW :: World -> World
stepW w = nub $ (w \\ dead w) `union` born w -- we leave a lot of dups, so nub here
    where
            dead w = filter (\c -> n c < 2 || n c > 3) w
            born w = filter (\c -> n c == 3) (neighborhood w)
            n c = length (cellNeighbors c `intersect` w)


