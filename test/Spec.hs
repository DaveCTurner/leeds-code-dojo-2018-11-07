module Main where

import Test.Hspec

import Data.Array

main :: IO ()
main = do
  hspec $ do
    describe "determine max grid size" $ do
      it "returns the max grid size from the  command" $ do
        maxGridSize [ON (2,3) (4,5)] `shouldBe`  (4, 5)
      it "returns the max grid size from the  command" $ do
        maxGridSize [ON (2,3) (4,5), OFF (8,9) (1,2)] `shouldBe`  (8, 9)
      it "returns the max grid size from the  command" $ do
        maxGridSize [] `shouldBe`  (0,0)

    describe "1x1 grids" $ do
      it "a 1 x 1 grid contains one on light" $ do
        processCommands [ON (0,0) (0,0)] `shouldBe` listArray ((0,0), (0,0)) [True]
      it "a 1 x 1 grid contains one off light" $ do
        processCommands [OFF (0,0) (0,0)] `shouldBe` listArray ((0,0), (0,0)) [False]
      it "a 1 x 1 grid contains toggles an off light to an on light" $ do
        processCommands [TOGGLE (0,0) (0,0)] `shouldBe` listArray ((0,0), (0,0)) [True]
      it "toggles a light on then off" $ do
        processCommands [TOGGLE (0,0) (0,0), TOGGLE (0,0) (0,0)] `shouldBe` listArray ((0,0), (0,0)) [False]
      it "can turn a light on then off" $ do
        processCommands [ON (0,0) (0,0), OFF (0,0) (0,0)] `shouldBe` listArray ((0,0), (0,0)) [False]
      it "returns a 1 x 1 grid with empty input" $ do
        processCommands [] `shouldBe` listArray ((0,0), (0,0)) [False]
    
    describe "2x1 grids" $ do
      it "can turn all lights on" $ do
        processCommands [ON (0,0) (1,0)] `shouldBe` listArray ((0,0), (1,0)) [True, True]
      it "can toggle all lights to on" $ do
        processCommands [TOGGLE (0,0) (1,0)] `shouldBe` listArray ((0,0), (1,0)) [True, True]
      it "can toggle a light on" $ do
        processCommands [ON (1,0) (1,0)] `shouldBe` listArray ((0,0), (1,0)) [False, True]

    describe "pretty printing arrays of lights" $ do
      it "can print a 1 x 1 grid" $ do
        showBoard (processCommands []) `shouldBe` "　\n"
      it "can print a 2 x 1 grid" $ do
        showBoard (processCommands [OFF (0,1) (0,1)]) `shouldBe` "　\n　\n"
      it "can print a 2 x 2 grid" $ do
        showBoard (processCommands [OFF (1,1) (1,1)]) `shouldBe` "　　\n　　\n"
      it "can print a 1 x 1 grid with a light on" $ do
        showBoard (processCommands [ON (0,0) (0,0)]) `shouldBe` "💡\n"
      it "can print a 2 x 2 grid with a light on" $ do
        showBoard (processCommands [OFF (1,1) (1,1), ON (1,0) (1,0)]) `shouldBe` "　💡\n　　\n"
      it "can print a 2 x 2 grid with two lights on" $ do
        showBoard (processCommands [ON (1,0) (1,1)]) `shouldBe` "　💡\n　💡\n"

  putStr $ showBoard $ processCommands $ inputCommands 10 6
  putStr $ showBoard $ processCommands $ inputCommands 30 30
  putStr $ showBoard $ processCommands $ inputCommands 30 6

type LightsBoard = Array (Int, Int) Bool

data Action = On | Off | Toggle deriving (Show, Eq)

data Command = ON     (Int,Int) (Int,Int)
             | OFF    (Int,Int) (Int,Int)
             | TOGGLE (Int,Int) (Int,Int)
  deriving (Show, Eq)

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