module Main where

import Data.List (find, intercalate)
import Control.Concurrent.Thread.Delay (delay)

data Tile = Head | Tail
data Dir = N | E | S | W

type Entity = (Tile, Int, Int)
type ViewportWidth = Int
type ViewportHeight = Int
data GameState = GameState Dir [Entity]

lookupTile :: [Entity] -> Int -> Int -> Char
lookupTile es x y = case find (\(_, x', y') -> x == x' && y == y') es of
  Nothing -> '.'
  Just (Head, _, _) -> 'H'
  Just (Tail, _, _) -> '*'

drawScreen :: GameState -> ViewportWidth -> ViewportHeight -> IO ()
drawScreen (GameState d es) w h = do
  let coords = [ (x,y) | x<-[0..w], y<-[0..h] ]
  let screen = map (\(x, y) -> lookupTile es x y) coords
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
