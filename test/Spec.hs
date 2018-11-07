module Main where

import Test.Hspec

import Data.Array

main :: IO ()
main = do
  hspec $ do
    describe "maxGridSize" $ do
      let shouldGive input expected = it (show input) $ maxGridSize input `shouldBe` expected
      [ON (2,3) (4,5)] `shouldGive` (4, 5)
      [ON (2,3) (4,5), OFF (8,9) (1,2)] `shouldGive` (8, 9)
      [] `shouldGive` (0,0)

    describe "processCommands" $ do
      let shouldGive input expected = it (show input) $ processCommands input `shouldBe` expected

      describe "(1x1)" $ do
        [ON (0,0) (0,0)] `shouldGive` listArray00 (0,0) [True]
        [OFF (0,0) (0,0)] `shouldGive` listArray00 (0,0) [False]
        [TOGGLE (0,0) (0,0)] `shouldGive` listArray00 (0,0) [True]
        [TOGGLE (0,0) (0,0), TOGGLE (0,0) (0,0)] `shouldGive` listArray00 (0,0) [False]
        [ON (0,0) (0,0), OFF (0,0) (0,0)] `shouldGive` listArray00 (0,0) [False]
        [] `shouldGive` listArray00 (0,0) [False]
      
      describe "(2x1)" $ do
        [ON (0,0) (1,0)]     `shouldGive` listArray00 (1,0) [True, True]
        [TOGGLE (0,0) (1,0)] `shouldGive` listArray00 (1,0) [True, True]
        [ON (1,0) (1,0)]     `shouldGive` listArray00 (1,0) [False, True]

    describe "showBoard" $ do
      let shouldGive input expected = it (show input) $ showBoard (processCommands input) `shouldBe` expected
      [] `shouldGive` "　\n"
      [OFF (0,1) (0,1)] `shouldGive` "　\n　\n"
      [OFF (1,1) (1,1)] `shouldGive` "　　\n　　\n"
      [ON (0,0) (0,0)] `shouldGive` "💡\n"
      [OFF (1,1) (1,1), ON (1,0) (1,0)] `shouldGive` "　💡\n　　\n"
      [ON (1,0) (1,1)] `shouldGive` "　💡\n　💡\n"

  putStr $ showBoard $ processCommands $ inputCommands 10 6
  putStr $ showBoard $ processCommands $ inputCommands 30 30
  putStr $ showBoard $ processCommands $ inputCommands 30 6

type LightsBoard = Array (Int, Int) Bool

data Action = On | Off | Toggle deriving (Show, Eq)

data Command = ON     (Int,Int) (Int,Int)
             | OFF    (Int,Int) (Int,Int)
             | TOGGLE (Int,Int) (Int,Int)
  deriving (Show, Eq)

listArray00 :: (Int,Int) -> [Bool] -> LightsBoard
listArray00 xy = listArray ((0,0),xy)

rectangle :: Command -> ((Int,Int),(Int,Int))
rectangle (ON     ab cd) = (ab,cd)
rectangle (OFF    ab cd) = (ab,cd)
rectangle (TOGGLE ab cd) = (ab,cd)

showBoard :: LightsBoard -> String
showBoard l = concatMap showRow [0..maxRow]
  where (_, (maxColumn, maxRow)) = bounds l
        showRow   r   = map (showLight r) [0..maxColumn] ++ "\n"
        showLight r c = if l ! (c,r) then '💡' else '　'

processCommands :: [Command] -> LightsBoard
processCommands commands = foldr processCommand (listArray ((0,0), maxGridSize commands) $ repeat False) $ reverse commands

processCommand :: Command -> LightsBoard -> LightsBoard
processCommand cmd currentBoard = accum changeLight currentBoard toUpdates
  where
  changeLight :: Bool -> () -> Bool
  changeLight b () = case cmd of 
    ON{}     -> True
    OFF{}    -> False
    TOGGLE{} -> not b

  toUpdates :: [((Int,Int),())]
  toUpdates = [(xy,()) | xy <- range (rectangle cmd)]

maxGridSize :: [Command] -> (Int, Int)
maxGridSize [] = (0,0)
maxGridSize commands = (maximum xs, maximum ys)
  where extract cmd = let ((a,b),(c,d)) = rectangle cmd in (max a c, max b d)
        xs = map (fst . extract) commands
        ys = map (snd . extract) commands

inputCommands :: Int -> Int -> [Command]
inputCommands w h =
  [ ON  (0,0)(w,h)
  , OFF (1,1)(w-1,h-1)
  , ON  (midPoint,maxRow+1)(midPoint,maxRow+1)
  ]
  ++ [ON (midPoint-r+1,r)(midPoint+r-1,r)
      | r <- [1..maxRow]]
  where
    midPoint = w `div` 2
    maxRow = min (h-2) (midPoint-1)