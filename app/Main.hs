module Main where

import Data.List (find, intercalate)
import Control.Concurrent.Thread.Delay (delay)

data Dir = N | E | S | W

type ViewportWidth = Int
type ViewportHeight = Int
data GameState = GameState Dir (Int, Int) [(Int, Int)]

lookupTile :: GameState -> Int -> Int -> Char
lookupTile (GameState _ (hx, hy) es) x y
  | (x, y) == (hx, hy) = 'H' -- head
  | elem (x, y) es = '*' -- tail
  | otherwise = '.'

drawScreen :: GameState -> ViewportWidth -> ViewportHeight -> IO ()
drawScreen gs w h = do
  let coords = [ (x, y) | x <- [0..w], y <- [0..h] ]
  let screen = map (\(x, y) -> lookupTile gs x y) coords
  let rows = map (\r -> map (\((x, y), c) -> c) $ filter (\((x, y), c) -> y == r) $ zip coords screen) [0..h]
  putStrLn (intercalate "\n" rows)

initialState :: GameState
initialState =
  GameState N [(Head, 0, 0), (Tail, 1, 0), (Tail, 2, 0), (Tail, 3, 0)]

update :: GameState -> GameState
update s = s

loop :: GameState -> ViewportWidth -> ViewportHeight -> IO ()
loop s w h = do
  drawScreen s w h
  delay 1000000
  loop (update s) w h

main = loop initialState 10 10
