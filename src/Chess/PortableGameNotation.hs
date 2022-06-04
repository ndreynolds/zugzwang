module Chess.PortableGameNotation
  ( parseCoord,
    parsePortableGameFile,
    parsePieceKind,
    parseMoveInput,
    move,
    tag,
  )
where

import Chess
  ( CastlingSide (..),
    Color (..),
    Coord,
    FileId (..),
    GameInput (..),
    MoveInput (..),
    MoveInputSpecial (..),
    Piece (..),
    PieceKind (..),
    RankId (..),
    Tag (..),
    startingBoard,
  )
import Data.Char (isSpace)
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Token

--
-- Helpers
--

doubleQuote = char '"'

-- Similar to Parsec's space, but excluding newlines.
space' :: Parsec String () Char
space' = satisfy (\ch -> isSpace ch && ch /= '\n')

spaces' :: Parsec String () ()
spaces' = skipMany space'

--
-- Tags
--
--  [Event "Third Rosenwald Trophy"]
--  [Site "New York, NY USA"]
--

tagNames =
  [ "Event",
    "Site",
    "Date",
    "Round",
    "WhiteElo",
    "BlackElo",
    "White",
    "Black",
    "Result",
    "ECO"
  ]

tag :: Parsec String () Tag
tag = do
  char '['
  tagType <- choice $ map (try . string) tagNames
  many1 space'
  tagContents <- between doubleQuote doubleQuote (many (noneOf "[]\""))
  char ']'
  return $ Tag (read ("Tag" ++ tagType)) tagContents

--
-- Comments
--
--  {This opening is called the Ruy Lopez.}
--
--  ; This opening is called the Ruy Lopez.

-- TODO: Support semicolon to EOL style.
comment :: Parsec String () String
comment = do
  spaces'
  between (char '{') (char '}') (many (noneOf "{}"))

--
-- Piece Kind
--
--   K, Q, R, B, N

pieceKind :: Parsec String () PieceKind
pieceKind = do
  kind <- oneOf "KQRBN"
  return $ case kind of
    'K' -> King
    'Q' -> Queen
    'R' -> Rook
    'B' -> Bishop
    'N' -> Knight

--
-- Coordinates
--
--   e3, a6, f, 8
--

partialCoord :: Parsec String () (Maybe FileId, Maybe RankId)
partialCoord = do
  maybeFile <- optionMaybe fileId
  maybeRank <- optionMaybe rankId
  return (maybeFile, maybeRank)

coord :: Parsec String () Coord
coord = do
  file <- fileId
  rank <- rankId
  return (file, rank)

rankId :: Parsec String () RankId
rankId = do
  raw <- oneOf "12345678"
  return $ case raw of
    '1' -> Rank1
    '2' -> Rank2
    '3' -> Rank3
    '4' -> Rank4
    '5' -> Rank5
    '6' -> Rank6
    '7' -> Rank7
    '8' -> Rank8

fileId :: Parsec String () FileId
fileId = do
  raw <- oneOf "abcdefgh"
  return $ case raw of
    'a' -> FileA
    'b' -> FileB
    'c' -> FileC
    'd' -> FileD
    'e' -> FileE
    'f' -> FileF
    'g' -> FileG
    'h' -> FileH

--
-- Move Annotations
--
--   x, +, #
--

capture :: Parsec String () MoveInputSpecial
capture = do char 'x'; return Capture

check :: Parsec String () MoveInputSpecial
check = do char '+'; return Check

checkmate :: Parsec String () MoveInputSpecial
checkmate = do char '#'; return Checkmate

--
-- Moves
--
--   e4, Nf3, Rxe1+, Nbd7
--

move :: Color -> Parsec String () MoveInput
move color =
  try (disambiguatedMove color)
    <|> try (ambiguousMove color)
    <|> castlingMove color

-- TODO: Promotion (e.g. e8=Q)

-- A move input without a deparature rank or file specified. Is ambiguous without
-- the current game state (which should disambiguate if valid).
ambiguousMove :: Color -> Parsec String () MoveInput
ambiguousMove color = do
  pieceKind <- option Pawn pieceKind
  maybeCapture <- optionMaybe capture
  destination <- coord
  maybeCheckOrCheckmate <- optionMaybe (check <|> checkmate)
  return $
    MoveInput
      (Piece pieceKind color)
      (Nothing, Nothing)
      destination
      (catMaybes [maybeCapture, maybeCheckOrCheckmate])

-- A move input that specifies a deparature rank and/or file.
disambiguatedMove :: Color -> Parsec String () MoveInput
disambiguatedMove color = do
  pieceKind <- option Pawn pieceKind
  origin <- partialCoord
  maybeCapture <- optionMaybe capture
  destination <- coord
  maybeCheckOrCheckmate <- optionMaybe (check <|> checkmate)
  return $
    MoveInput
      (Piece pieceKind color)
      origin
      destination
      (catMaybes [maybeCapture, maybeCheckOrCheckmate])

-- TODO: Support special moves
castlingMove :: Color -> Parsec String () MoveInput
castlingMove color = do
  val <- try (string "O-O-O") <|> string "O-O"
  maybeCheckOrCheckmate <- optionMaybe (check <|> checkmate)
  return $ case val of
    "O-O" -> CastlingMoveInput color Kingside (catMaybes [maybeCheckOrCheckmate])
    "O-O-O" -> CastlingMoveInput color Queenside (catMaybes [maybeCheckOrCheckmate])

--
-- Move Groups
--
--   1. e4 e5
--   20. Nbd2 Nxd6
--   43. Re6 1/2-1/2

movePairOrSingleMove :: Parsec String () [MoveInput]
movePairOrSingleMove =
  try movePair <|> try singleMove

movePair :: Parsec String () [MoveInput]
movePair = do
  moveNumber
  moveA <- move White
  many1 space'
  optional (try comment)
  moveB <- move Black
  -- parserTraced "comment" (try $ optional comment)
  optional (try endOfGame)

  return [moveA, moveB]

singleMove :: Parsec String () [MoveInput]
singleMove = do
  moveNumber
  move' <- move White
  optional (try comment)
  many space'
  endOfGame -- If single, must be the final move and include end of game result.
  return [move']

-- TODO: Enforce increment?
moveNumber :: Parsec String () Int
moveNumber = do
  num <- many1 digit
  char '.'
  many space'
  return (read num :: Int)

endOfGame :: Parsec String () String
endOfGame = do
  many space'
  choice
    ( map
        try
        [ string "0-1",
          string "1-0",
          string "0-0",
          string "1/2-1/2",
          string "1/2-0",
          string "0-1/2"
        ]
    )

--
-- Portable Game Notation files
--
--   [Event "F/S Return Match"]
--   [Site "Belgrade, Serbia JUG"]
--   [Date "1992.11.04"]
--   [Round "29"]
--   [White "Fischer, Robert J."]
--   [Black "Spassky, Boris V."]
--   [Result "1/2-1/2"]
--
--   1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
--   4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
--   11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
--   Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
--   23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
--   hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
--   35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
--   Nf2 42. g4 Bd3 43. Re6 1/2-1/2

portableGame :: Parsec String () GameInput
portableGame = do
  tags <- sepEndBy tag (many1 endOfLine)
  many endOfLine
  nestedMoveInputs <- sepEndBy1 movePairOrSingleMove (many1 endOfLine <|> many1 space)
  return GameInput {moveInputs = concat nestedMoveInputs, tags = tags}

portableGameFile :: Parsec String () [GameInput]
portableGameFile = many1 portableGame

parsePortableGameFile :: String -> Either ParseError [GameInput]
parsePortableGameFile = parse portableGameFile "(unknown)"

parseMoveInput :: Color -> String -> Either ParseError MoveInput
parseMoveInput color = parse (move color) "(unknown)"

parsePieceKind :: String -> Either ParseError PieceKind
parsePieceKind = parse pieceKind "(unknown)"

parseCoord :: String -> Either ParseError Coord
parseCoord = parse coord "(unknown)"