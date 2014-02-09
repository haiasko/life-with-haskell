-- | Main entry point to the application.
module Main where

import Data.List (union,intersect,(\\),nub)

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Conway's Life"
    loop $ frame twoGliders
    
loop :: World -> IO ()
loop w = do
    putStrLn $ showW w
    getLine
    if length w > 0 then
        loop $ stepW w
    else    
        return ()
    
-- simplest impl - a list of live cells
type Cell = (Int, Int)
type World = [Cell]

-- samples
anchor = [(0, 0), (0, 1), (1, 0), (1, 1)]
lighthouse = [(0, 0), (0, 1), (0, 2)]
glider = [(0,0), (1,0), (2,0), (0,1), (1,2)]
twoGliders = glider ++ (mirror $ offset glider (4, 5))

-- move a pat by a cells origin :)
offset :: World -> Cell -> World
offset w (x, y) = [(cx + x, cy +y) | (cx, cy) <- w]

-- rotate a pat
-- mirror a pat
mirror :: World -> World
mirror w = [(-x, -y) |  (x, y) <- w]

-- frame smth with an anchor upper left and a lighthouse down right
frame w = offset anchor (minX - 5, minY - 5) ++ w ++ offset lighthouse (maxX + 5, maxY + 5)
    where
        (minX, minY, maxX, maxY) = getWindow w
    

showCell :: World -> Cell -> Char 
showCell w c
    | c `elem` w = 'O'
    | c `elem` neighb = '_'
    | otherwise = '.'
    where
        neighb = neighborhood w

-- shows the whole world (min to max)
showW :: World -> String
showW [] = "empty world"
showW w =
    show (minX, minY) ++ " " ++ show (maxX, maxY) ++ "\n" ++ 
         concat [showLine w y | y <- [minY..maxY]]
    where
        showLine w l = [showCell w (x, l) | x <- [minX..maxX]] ++ "\n"
        (minX, minY, maxX, maxY) = getWindow w
        
getWindow :: World -> (Int, Int, Int, Int)
getWindow [] = (0, 0, 20, 10) -- annihilation is not uncommon, then Prelude.minimum bombs on me cuz of empty world
getWindow w = (minX, minY, maxX, maxY)
    where
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


