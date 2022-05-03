module Main where

import Chess
  ( FileId (..),
    Piece (..),
    PieceKind (..),
    RankId (..),
    Color (..),
    displayBoard,
    movements,
    movesBoard,
    startingBoard,
  )

main :: IO ()
main = do
  putStrLn $ displayBoard $ movesBoard (Piece Queen Black) (FileD, Rank8) 
  putStrLn ""
  putStrLn $ displayBoard $ movesBoard (Piece King Black) (FileD, Rank5) 
  putStrLn ""
  putStrLn $ displayBoard $ movesBoard (Piece Knight Black) (FileD, Rank5) 
  putStrLn ""
  putStrLn $ displayBoard $ movesBoard (Piece Bishop Black) (FileD, Rank5) 
  -- print $ movements (Piece Knight White) (FileB, Rank1) startingBoard