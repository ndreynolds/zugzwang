{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Chess
  ( Color (..),
    FileId (..),
    Game (..),
    GameInput (..),
    Piece (..),
    PieceKind (..),
    RankId (..),
    applyMoveInput,
    displayBoard,
    movements,
    movesBoard,
    runGameInput,
    startingBoard,
    startingGame,
  )
import Chess.PortableGameNotation
  ( parseCoord,
    parseMoveInput,
    parsePieceKind,
    parsePortableGameFile,
  )
import Data.Either
import System.Console.CmdArgs

data Subcommand
  = Moves {piece :: String, from :: String}
  | Parse {path :: FilePath}
  | Play {}
  deriving (Data, Typeable, Show, Eq)

movesMode =
  Moves
    { piece = def &= help "Piece to show movements for",
      from = def &= help "File and rank of the piece"
    }

parseMode =
  Parse
    { path = def &= help "Path to a file in Portable Game Notation (PGN) format" &= typFile
    }

playMode =
  Play {}

mode = cmdArgs $ modes [movesMode, parseMode, playMode]

nextColor :: Color -> Color
nextColor White = Black
nextColor Black = White

playGame :: Color -> Game -> IO (Either String Game)
playGame color game@Game {board = board} = do
  putStrLn $ displayBoard board
  putStrLn ("Enter move (" ++ show color ++ "):")
  rawMoveInput <- getLine
  case parseMoveInput color rawMoveInput of
    Left error -> do print error; playGame color game
    Right moveInput ->
      case applyMoveInput game moveInput of
        Left error ->
          do print error; playGame color game
        Right game ->
          playGame (nextColor color) game

run :: Subcommand -> IO ()
run (Moves {piece = piece, from = from}) = do
  let pieceKind = fromRight King (parsePieceKind piece)
  let coord = fromRight (FileD, Rank5) (parseCoord from)
  let board = movesBoard (Piece pieceKind Black) coord
  putStrLn $ displayBoard board
run (Parse {path = path}) = do
  contents <- readFile path
  case parsePortableGameFile contents of
    Left error ->
      print error
    Right gameInputs ->
      print $ map runGameInput gameInputs
run (Play {}) = do
  game <- playGame White startingGame
  print game

main = do
  opts <- mode
  run opts
