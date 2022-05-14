module Chess
  ( Game (..),
    GameInput (..),
    Board,
    Rank,
    Square (..),
    Color (..),
    Piece (..),
    PieceKind (..),
    RankId (..),
    FileId (..),
    Tag (..),
    Move (..),
    MoveInput (..),
    MoveInputSpecial (..),
    CastlingSide (..),
    Coord,
    startingBoard,
    movesBoard,
    displayBoard,
    movements,
    allSquares,
    resolveMoveInput,
    runGameInput,
  )
where

import Control.Lens
import Control.Monad
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Maybe
import Debug.Trace (trace, traceShowId)

--
-- Data Types
--
data Color
  = White
  | Black
  deriving (Show, Eq)

data PieceKind
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Show, Read, Eq, Enum, Bounded)

data Piece = Piece PieceKind Color
  deriving (Show, Eq)

type Board =
  ( Rank,
    Rank,
    Rank,
    Rank,
    Rank,
    Rank,
    Rank,
    Rank
  )

type Rank =
  ( Square,
    Square,
    Square,
    Square,
    Square,
    Square,
    Square,
    Square
  )

data Square
  = EmptySquare
  | OccupiedSquare Piece
  | MarkedSquare -- E.g. for displaying moves
  deriving (Show)

data Move
  = Move Piece Coord Coord
  | CastlingMove Color CastlingSide
  deriving (Show)

-- Some move input (e.g. Standard Algebraic Notation) is ambiguous
-- without the current game state.
data MoveInput
  = MoveInput Piece OptionalCoord Coord [MoveInputSpecial]
  | CastlingMoveInput Color CastlingSide [MoveInputSpecial]
  deriving (Show)

data CastlingSide
  = Queenside
  | Kingside
  deriving (Show)

data MoveInputSpecial
  = Check
  | Checkmate
  | Capture
  | Promotion PieceKind
  deriving (Show)

data Tag = Tag TagType String
  deriving (Show)

data TagType
  = TagEvent
  | TagSite
  | TagDate
  | TagRound
  | TagWhite
  | TagBlack
  | TagResult
  deriving (Show, Read, Enum, Bounded)

data Game = Game
  { board :: Board,
    moves :: [Move]
  }
  deriving (Show)

data GameInput = GameInput
  { moveInputs :: [MoveInput],
    tags :: [Tag]
  }
  deriving (Show)

data RankId
  = Rank1
  | Rank2
  | Rank3
  | Rank4
  | Rank5
  | Rank6
  | Rank7
  | Rank8
  deriving (Show, Eq, Enum, Bounded)

data FileId
  = FileA
  | FileB
  | FileC
  | FileD
  | FileE
  | FileF
  | FileG
  | FileH
  deriving (Show, Eq, Enum, Bounded)

type Coord = (FileId, RankId)

-- May specify file+rank, only file, only rank, or nothing.
type OptionalCoord = (Maybe FileId, Maybe RankId)

--
-- Board Construction
--
rankOf :: Square -> Rank
rankOf sq = (sq, sq, sq, sq, sq, sq, sq, sq)

boardOf :: Rank -> Board
boardOf r = (r, r, r, r, r, r, r, r)

emptyRank :: Rank
emptyRank = rankOf EmptySquare

pawnRank :: Color -> Rank
pawnRank color = rankOf $ OccupiedSquare $ Piece Pawn color

pieceRank :: Color -> Rank
pieceRank color =
  ( OccupiedSquare $ Piece Rook color,
    OccupiedSquare $ Piece Knight color,
    OccupiedSquare $ Piece Bishop color,
    OccupiedSquare $ Piece Queen color,
    OccupiedSquare $ Piece King color,
    OccupiedSquare $ Piece Bishop color,
    OccupiedSquare $ Piece Knight color,
    OccupiedSquare $ Piece Rook color
  )

startingBoard :: Board
startingBoard =
  ( pieceRank Black,
    pawnRank Black,
    emptyRank,
    emptyRank,
    emptyRank,
    emptyRank,
    pawnRank White,
    pieceRank White
  )

emptyBoard :: Board
emptyBoard = boardOf emptyRank

startingGame :: Game
startingGame = Game {board = startingBoard, moves = []}

movesBoard :: Piece -> Coord -> Board
movesBoard piece coord = foldr markSquare board (movements piece coord board)
  where
    board = putSquare coord (OccupiedSquare piece) emptyBoard

allRankIds :: [RankId]
allRankIds = enumFrom minBound

allFileIds :: [FileId]
allFileIds = enumFrom minBound

allCoords :: [Coord]
allCoords = [(file, rank) | rank <- reverse allRankIds, file <- allFileIds]

allTagTypes :: [TagType]
allTagTypes = enumFrom minBound

-- Lenses

rankLens rankId =
  case rankId of
    Rank1 -> _8
    Rank2 -> _7
    Rank3 -> _6
    Rank4 -> _5
    Rank5 -> _4
    Rank6 -> _3
    Rank7 -> _2
    Rank8 -> _1

fileLens fileId =
  case fileId of
    FileA -> _1
    FileB -> _2
    FileC -> _3
    FileD -> _4
    FileE -> _5
    FileF -> _6
    FileG -> _7
    FileH -> _8

coordLens (fileId, rankId) = rankLens rankId . fileLens fileId

--
-- Moves
--

runGameInput :: GameInput -> Either (String, Game) Game
runGameInput GameInput {moveInputs = moveInputs} = applyMoveInputs startingGame moveInputs

applyMoveInputs :: Game -> [MoveInput] -> Either (String, Game) Game
applyMoveInputs = foldM applyMoveInput

applyMove :: Game -> Move -> Either (String, Game) Game
applyMove game@Game {board = board, moves = moves} move@(Move piece origin dest)
  | isMoveAllowed board dest (piece, origin) =
    Right $
      trace (displayBoard $ movePiece board) game {board = movePiece board, moves = move : moves}
  | otherwise =
    Left ("Move not allowed for " ++ show piece ++ " at " ++ show origin, game)
  where
    movePiece board = putSquare origin EmptySquare (putSquare dest (OccupiedSquare piece) board)

applyMoveInput :: Game -> MoveInput -> Either (String, Game) Game
applyMoveInput game moveInput =
  case resolveMoveInput (board game) moveInput of
    Left error -> Left (error, game)
    Right move -> applyMove game move

resolveMoveInput :: Board -> MoveInput -> Either String Move
resolveMoveInput board (MoveInput piece optionalOrigin dest special) =
  case filter moveAllowed candidates of
    [] -> Left "Unable to find"
    [(piece, resolvedOrigin)] -> Right (Move piece resolvedOrigin dest)
  where
    moveAllowed = isMoveAllowed board dest
    candidates = findPieces board (piece, optionalOrigin)
resolveMoveInput board _ = Left "unhandled" -- TODO: castling

getSquare :: Coord -> Board -> Square
getSquare coord = view (coordLens coord)

putSquare :: Coord -> Square -> Board -> Board
putSquare coord = set (coordLens coord)

emptySquare :: Coord -> Board -> Board
emptySquare coord = putSquare coord EmptySquare

markSquare :: Coord -> Board -> Board
markSquare coord = putSquare coord MarkedSquare

isCoordOccupied :: Board -> Coord -> Bool
isCoordOccupied board coord = case getSquare coord board of
  OccupiedSquare _ -> True
  _ -> False

isSquareOccupied :: Square -> Bool
isSquareOccupied (OccupiedSquare _) = True
isSquareOccupied _ = False

allSquares :: Board -> [(Square, Coord)]
allSquares board = zip squares allCoords
  where
    squares = concatMap (^.. each) ((^.. each) board)

allPieces :: Board -> [(Piece, Coord)]
allPieces board = map (first justPiece) occupiedSquares
  where
    occupiedSquares = filter (\(s, c) -> isSquareOccupied s) (allSquares board)

justPiece :: Square -> Piece
justPiece (OccupiedSquare piece) = piece

findPieces :: Board -> (Piece, OptionalCoord) -> [(Piece, Coord)]
findPieces board (piece, partialCoord) =
  filter
    (\(p, c) -> p == piece && isPartialCoordMatch partialCoord c)
    (allPieces board)

isPartialCoordMatch :: (Maybe FileId, Maybe RankId) -> Coord -> Bool
isPartialCoordMatch (Nothing, Nothing) _ = True
isPartialCoordMatch (Just file, Nothing) (f, _) = file == f
isPartialCoordMatch (Nothing, Just rank) (_, r) = rank == r
isPartialCoordMatch (Just file, Just rank) (f, r) = file == f && rank == r

isMoveAllowed :: Board -> Coord -> (Piece, Coord) -> Bool
isMoveAllowed board dest (piece, origin) = dest `elem` movements piece origin board

-- TODO: castle
movements :: Piece -> Coord -> Board -> [Coord]
movements (Piece King _) coord board =
  concatMap
    (moveOnce coord board)
    [ up,
      down,
      left,
      right,
      upLeft,
      upRight,
      downLeft,
      downRight
    ]
movements (Piece Queen _) coord board =
  concatMap
    (moveMultiple coord board)
    [ up,
      down,
      left,
      right,
      upLeft,
      upRight,
      downLeft,
      downRight
    ]
movements (Piece Rook _) coord board =
  concatMap
    (moveMultiple coord board)
    [ up,
      down,
      left,
      right
    ]
movements (Piece Bishop _) coord board =
  concatMap
    (moveMultiple coord board)
    [ upLeft,
      upRight,
      downLeft,
      downRight
    ]
movements (Piece Knight _) coord board =
  concatMap
    (moveOnce coord board)
    [ right >=> down >=> down,
      right >=> up >=> up,
      left >=> down >=> down,
      left >=> up >=> up,
      right >=> right >=> up,
      right >=> right >=> down,
      left >=> left >=> up,
      left >=> left >=> down
    ]
-- TODO: en passant
movements (Piece Pawn White) coord board =
  case coord of
    (_, Rank2) -> moveTwice coord board up
    _ -> moveOnce coord board up
movements (Piece Pawn Black) coord board =
  case coord of
    (_, Rank7) -> moveTwice coord board down
    _ -> moveOnce coord board down

moveOnce :: Coord -> Board -> (Coord -> Maybe Coord) -> [Coord]
moveOnce coord board f = case f coord of
  Just coord' -> [coord' | not (isCoordOccupied board coord')]
  Nothing -> []

moveTwice :: Coord -> Board -> (Coord -> Maybe Coord) -> [Coord]
moveTwice coord board f = case moveOnce coord board f of
  [] -> []
  [coord'] -> coord' : moveOnce coord' board f

moveMultiple :: Coord -> Board -> (Coord -> Maybe Coord) -> [Coord]
moveMultiple coord board f = case moveOnce coord board f of
  [] -> []
  [coord'] -> coord' : moveMultiple coord' board f

up :: Coord -> Maybe Coord
up (f, r) = if r == maxBound then Nothing else Just (f, succ r)

down :: Coord -> Maybe Coord
down (f, r) = if r == minBound then Nothing else Just (f, pred r)

left :: Coord -> Maybe Coord
left (f, r) = if f == minBound then Nothing else Just (pred f, r)

right :: Coord -> Maybe Coord
right (f, r) = if f == maxBound then Nothing else Just (succ f, r)

upLeft :: Coord -> Maybe Coord
upLeft coord = left =<< up coord

upRight :: Coord -> Maybe Coord
upRight coord = right =<< up coord

downLeft :: Coord -> Maybe Coord
downLeft coord = left =<< down coord

downRight :: Coord -> Maybe Coord
downRight coord = right =<< down coord

--
-- Display
--
displayBoard :: Board -> String
displayBoard (r8, r7, r6, r5, r4, r3, r2, r1) =
  intercalate
    "\n"
    [ "   A  B  C  D  E  F  G  H ",
      "8 " ++ displayRank r8 8,
      "7 " ++ displayRank r7 7,
      "6 " ++ displayRank r6 6,
      "5 " ++ displayRank r5 5,
      "4 " ++ displayRank r4 4,
      "3 " ++ displayRank r3 3,
      "2 " ++ displayRank r2 2,
      "1 " ++ displayRank r1 1
    ]

displayRank :: Rank -> Integer -> String
displayRank (fA, fB, fC, fD, fE, fF, fG, fH) y =
  intercalate
    ""
    [ displaySquare fA (1, y),
      displaySquare fB (2, y),
      displaySquare fC (3, y),
      displaySquare fD (4, y),
      displaySquare fE (5, y),
      displaySquare fF (6, y),
      displaySquare fG (7, y),
      displaySquare fH (8, y)
    ]

displaySquare :: Square -> (Integer, Integer) -> String
displaySquare EmptySquare = displaySquare' Nothing
displaySquare (OccupiedSquare piece) = displaySquare' $ Just (displayPiece piece)
displaySquare MarkedSquare = displaySquare' $ Just (colorMagenta " ● ")

displaySquare' :: Maybe String -> (Integer, Integer) -> String
displaySquare' Nothing (x, y) = displaySquare' (Just "   ") (x, y)
displaySquare' (Just s) (x, y)
  | even x && even y || odd x && odd y = backgroundRed s
  | otherwise = backgroundYellow s

displayPiece :: Piece -> String
displayPiece (Piece kind White) = colorWhite [' ', pieceSymbol kind, ' ']
displayPiece (Piece kind Black) = colorBlack [' ', pieceSymbol kind, ' ']

pieceSymbol :: PieceKind -> Char
pieceSymbol Pawn = '♟'
pieceSymbol Rook = '♜'
pieceSymbol Knight = '♞'
pieceSymbol Bishop = '♝'
pieceSymbol Queen = '♛'
pieceSymbol King = '♚'

colorWhite = wrapAnsi "\x1b[37m"

colorBlack = wrapAnsi "\x1b[30m"

colorMagenta = wrapAnsi "\x1b[35m"

backgroundRed = wrapAnsi "\x1b[41m"

backgroundYellow = wrapAnsi "\x1b[43m"

wrapAnsi :: String -> String -> String
wrapAnsi code s = code ++ s ++ "\x1b[0m"