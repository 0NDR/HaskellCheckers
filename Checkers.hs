--Author: Christopher Smith & Taylor White
--This is the logic meant to facilitate a checkers game 
--played by two people on the same computer

{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}


module Checkers where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Map (Map)
import Data.Aeson -- for ToJSON
import Data.Aeson.Types
import Data.Aeson.Text             (encodeToLazyText )

-- Create the types needed to faclitate a game of checkers
type Position = (Int , Int) -- Tuple representing grow column
type LocationMap = Map Position PieceType -- This is our in dictionary form

data Player = Player PieceType deriving (Eq)
data Board = Board LocationMap deriving(Eq)
data PieceType = None | Red | Black | King PieceType deriving(Eq)-- b,B, r,R for regulars and kings 
data Move = Move Position Position deriving (Eq, Read)
data Game = Game GameTurn [Player] deriving (Eq)
data GameTurn = GameTurn Board Player deriving (Eq)

data GameJSON = GameJSON [LocationJSON] PieceType
data LocationJSON = LocationJSON {x :: Int, y :: Int, t :: PieceType}

instance ToJSON PieceType where
   toJSON  t = object ["type" .= showPieceType t]
instance ToJSON Player where
   toJSON (Player piecetype) = toJSON piecetype
instance ToJSON Move where
    toJSON (Move posA posB) = object ["FromX" .= fst posA, "FromY" .= snd posA, "ToX" .= fst posB, "ToY" .= snd posB]

getJSONfromLocation :: (Position, PieceType) -> Value
getJSONfromLocation (pos, t) = object["x" .= fst pos,"y" .= snd pos, "type" .= showPieceType t]

getJSONLocationMap :: LocationMap -> Value
getJSONLocationMap l = ret where
    listLoc = Map.toList l
    ret = toJSON $ map getJSONfromLocation listLoc

instance ToJSON Board where
    toJSON b = getJSONLocationMap $ piecesOnBoard b  
    
instance ToJSON GameTurn where
    toJSON (GameTurn board player) = object["activeTurn" .= toJSON player, "board" .= toJSON board]

instance ToJSON Game where
    toJSON (Game turn _) = toJSON turn

    
    
instance FromJSON Move where
        parseJSON = withObject "Move" $ \o -> do
            xf <- o .: "FromX"
            yf <- o .: "FromY"
            xt <- o .: "ToX"
            yt <- o .: "ToY"
            return  (Move (xf, yf) (xt, yt))
        

instance FromJSON PieceType where
        parseJSON (Object v) = getPieceType <$> (v .: "type")

instance FromJSON GameJSON where
        parseJSON = withObject "Game" $ \o -> do
            board <- o .: "board"
            turn <-  o .: "activeTurn"
            return  (GameJSON board turn)
        
instance FromJSON LocationJSON where
        parseJSON = withObject "tuple" $ \o -> do
            xPos <- o .: "x"
            yPos <- o .: "y"
            piece <- o .: "type"
            return  (LocationJSON xPos yPos (getPieceType piece))
    

getLocationFromLocationJSON :: LocationJSON -> (Position, PieceType)
getLocationFromLocationJSON (LocationJSON x y t) = ((x,y),t)

getLocationMapFromLocationJSONS :: [LocationJSON] -> LocationMap
getLocationMapFromLocationJSONS l = Map.fromList (map getLocationFromLocationJSON l)

getGameFromGameJSON :: GameJSON -> Game
getGameFromGameJSON (GameJSON a b) = (Game (GameTurn (Board (getLocationMapFromLocationJSONS a)) (Player b)) [Player Red, Player Black])
    
 
getPieceType :: String -> PieceType
getPieceType str = case str of
                    "." -> None
                    "b" -> Black
                    "r" -> Red
                    "B" -> King Black
                    "R" -> King Red
                    
showPieceType :: PieceType -> String
showPieceType piece = case piece of 
                      None       -> "."
                      Black      -> "b"
                      Red        -> "r"
                      King Black -> "B"
                      King Red   -> "R"
                      _          -> "?"
-- Return list of all positions on board
boardPositions = [(c, r) | r <- [1..8], c <- [1..8]]


--returns starting positions of red and black pieces

--oddCol = [(i*2) | i <-[1..4]]--columns where pieces are placed on every even position
--evenCol = [(i*2-1) | i <-[1..4]]--columns where pieces are placed on every odd position

--redPositions = [(c, r) | r <- [1..3], c <- [oddCol, evenCol, oddCol]]--black and red starting positions
--blackPositions = [(c, r) | r <- [6..8], c <- [evenCol, oddCol, evenCol]]
--Tells what is in a given space
inSpace :: LocationMap -> Position -> PieceType
inSpace lm p = maybe None id (Map.lookup p lm)


--Swaps two spaces with each other (used in moving as a piece is replaced by a empty space when it moves)
swap :: LocationMap -> Position -> Position -> LocationMap
swap lm a b = new where
   mrk = inSpace lm -- Sets up the call to inSpace
   new = Map.insert a (mrk b) $ Map.insert b (mrk a) lm --Calls inserts whats current in pos a into pos b and vice versa
--Lets us know if a position is actually on the board or not
inBounds :: Position -> Bool
inBounds pos = px > 0 && px<=8 && py > 0 && py <= 8 where
   (px,py) = pos

defBoard = Board defLocationMap -- Creates a board with a default location map(8x8)
defLocationMap = Map.fromList $ zip boardPositions (repeat None) --Maps every location on the board to none

getPlayerTurn :: Game -> PieceType
getPlayerTurn (Game (GameTurn b (Player p)) [Player Red, Player Black]) = p

getStartBoard :: Board
getStartBoard = startBoard where
    empty = defBoard
    redBoard = updateLocationMap empty (Map.fromList $ zip redPositions (repeat Red))
    startBoard = updateLocationMap redBoard (Map.fromList $ zip blackPositions (repeat Black))
    blackPositions = genPos [1..3]
    redPositions = genPos [6..8]
    genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]

getGameFromGameTurn :: GameTurn -> Game
getGameFromGameTurn turn = Game turn [Player Red, Player Black]
    
getStartGame :: Game
getStartGame = startGame where
    turn = GameTurn getStartBoard $ Player Black
    startGame = getGameFromGameTurn turn
    
getBoardFromGame :: Game -> Board
getBoardFromGame (Game (GameTurn b p) _) = b 
--returns a location and piece paired together
pairWithLocation :: PieceType -> Int -> Int -> (Position, PieceType)
pairWithLocation p x y = ((x,y),p)

--Returns a boards location map
piecesOnBoard :: Board -> LocationMap
piecesOnBoard (Board p) = p

--updates pieces on a board
updatePieces :: Board -> LocationMap -> Board
updatePieces (Board _) lm = Board lm


--updates with a location map
updateLocationMap :: Board -> LocationMap -> Board
updateLocationMap b locMap  = updatedBoard where
    ponb = piecesOnBoard b
    updatedBoard = updatePieces b (Map.union locMap ponb)

    
    
--Updates a specific position on the board with a new piece
updateLocation :: Board -> Position -> PieceType -> Board
updateLocation b pos p = updatedBoard where 
   ponb = piecesOnBoard b --Gets current location map
   updatedBoard = updatePieces b newLocationMap -- Creates new board where..
   newLocationMap = Map.insert pos p ponb -- Location map has piece at certain point

   
-- returns all empty positions on the board
emptyPositions  :: Board -> [Position]
emptyPositions (Board locMap) = Map.keys $ Map.filter (==None) locMap
-- Returns a non-king version of a piece
basicPiece :: PieceType -> PieceType
basicPiece (King p) = basicPiece p
basicPiece Red = Red
basicPiece Black = Black
basicPiece None = None

-- Checks to see if anyone has won yet
hasWon :: Board -> PieceType -> Bool
hasWon _ None = False --Handles 
hasWon b (King pieceType) = hasWon b pieceType --Handles it being called using a king piece
hasWon (Board lMap) p = ((not otherPieceThere) || (otherPieceMoves == [])) where
   otherPieceThere = any (eqType otherType) elems  -- there are no enemy pieces left
   otherPieceMoves = (getAllTurnMoves (GameTurn (Board lMap) (Player otherType))) --the enemy cannot move anywhere
   otherType = basicPiece p
   eqType p a = basicPiece a == p
   elems = Map.elems lMap

winReason :: Game -> String
winReason (Game(GameTurn (Board b) (Player p)) _) = reason where
   otherPieceThere = any (eqType otherType) elems  -- there are no enemy pieces left
   otherPieceMoves = (getAllTurnMoves (GameTurn (Board b) (Player otherType))) --the enemy cannot move anywhere
   otherType = basicPiece p
   eqType p a = basicPiece a == p
   elems = Map.elems b
   reason = if (not otherPieceThere) then
                "Conquest"
                                     else
                "Trapped"

-- Returns the current board for the game
gameBoard :: Game -> Board
gameBoard (Game(GameTurn board _)_) = board

--Returns if a game has ended due to someone winning
gameEnded :: Game -> Bool
gameEnded game = any(hasWon board) pieces where
   board = gameBoard game
   pieces = [Red, Black]

-- Returns the winner of a game
theWinner :: Game -> PieceType
theWinner game@(Game(GameTurn b (Player m)) _) = winner where
   winner = if gameOver game then
                             if (winReason game) == "Conquest" then
                                if m == Red then Black
                                            else Red
                                else
                                m
                            else None

-- Clears the position when piece is captured
clearTurnPos :: GameTurn -> Position -> GameTurn
clearTurnPos orig posSrc = replaceTurnPos orig posSrc None

replaceTurnPos :: GameTurn -> Position -> PieceType -> GameTurn
replaceTurnPos orig@(GameTurn ogBoard ogPlayer) posSrc nPiece =
   GameTurn nBoard ogPlayer where
      nBoard = updateLocation ogBoard posSrc nPiece

gameOver :: Game -> Bool
gameOver game = any (hasWon board) pieces where
  board = gameBoard game
  pieces = [Red, Black]
  
getTurnPieceAt :: GameTurn -> Position -> PieceType
getTurnPieceAt (GameTurn b _) p =
   inSpace (piecesOnBoard b) p

-- Makes a move from one position to the next
updateTurn :: GameTurn -> Position -> Position -> GameTurn
updateTurn orig@(GameTurn ogBoard ogPlayer) posSrc posDes = 
   GameTurn newBoard next where
      sourcepiece = inSpace (piecesOnBoard ogBoard) posSrc
      removeBoard = updateLocation ogBoard posSrc None
      newBoard = updateLocation removeBoard posDes sourcepiece
      next = case ogPlayer of
         Player Black -> Player Red
         Player (King Black) -> Player Red
         _ -> Player Black

-- Checks if a space is empty
-- isEmptySpace :: Board -> Position -> Bool
-- isEmptySpace b p  = space == None where
--    space = inSpace (piecesOnBoard b) 

-- Give the valid moves for the piece at a given location on the board
moveOptions :: Board -> Position -> [Position]
moveOptions b p = posWalk b p ++ posJump b p
-- Returns the possible basic moves a given piece at a location can make
posWalk :: Board -> Position -> [Position]
posWalk b p = filter (emptyInBounds b) (possibleMoves piece) where
   piece = inSpace (piecesOnBoard b) p
   (x, y) = p
   possibleMoves (King _) = possibleMoves Black ++ possibleMoves Red  --king can move anywhere RED and BLACK can move
   possibleMoves Black = [(x-1,y+1), (x+1,y+1)]                       -- Black can move left&down or right&down
   possibleMoves Red = [(x-1,y-1), (x+1,y-1)]                         -- Red can move left&up or left&down
   possibleMoves _ = []                                               -- an empty space cannot move

isTowards :: PieceType -> Position ->Position -> Bool
isTowards Red (_, sy) (_, dy)   = dy > sy
isTowards Black (_, sy) (_, dy) = dy < sy

emptyInBounds board pos = inBounds pos && boardEmptyAt board pos
-- Returns if the board is empty at a certain position
boardEmptyAt :: Board -> Position -> Bool
boardEmptyAt b p = m == None where
   m = inSpace (piecesOnBoard b) p

posJump :: Board -> Position -> [Position]
posJump b p = jumpPos where
   jumpPos = filter (emptyInBounds b ) (possibleMoves piece)
   piece = inSpace (piecesOnBoard b) p
    
   possibleMoves :: PieceType -> [Position]
   possibleMoves Black        = possibleMovesColor Black
   possibleMoves Red          = possibleMovesColor Red
   possibleMoves (King color) = possibleJumps
   possibleMoves _            = []
   
   possibleMovesColor :: PieceType -> [Position]
   possibleMovesColor color = filter towards moves where
      towards = isTowards (toggleColor color) p
      moves = possibleJumps
        
   possibleJumps = map(\(_, a, _) -> a) jumps where
      jumps = filter canJump possibleJumpTuples
      possibleJumpTuples = zip3 diag1 diag2 piecesAtDiagonals
      diag1 = diagonals p
      diag2 = diagonalN 2 p
      piecesAtDiagonals = map (inSpace (piecesOnBoard b)) diag1
      
      canJump :: (Position, Position, PieceType) -> Bool
      canJump (_, _, pt) = pieceMayJumpPiece piece pt
-- returns passed piece in opposing colour

toggleColor :: PieceType -> PieceType
toggleColor Black        = Red
toggleColor Red          = Black
toggleColor (King color) = King $ toggleColor color
toggleColor c            = c

pieceMayJumpPiece :: PieceType -> PieceType -> Bool
pieceMayJumpPiece None _ = False
pieceMayJumpPiece piece (King p) = p /= None && pieceMayJumpPiece piece p
pieceMayJumpPiece piece Red = piece /= Red
pieceMayJumpPiece piece Black = piece /= Black
pieceMayJumpPiece _ _ = False


diagonalN :: Int -> Position -> [Position]
diagonalN d (r, c) = [(r+d, c+d), (r-d, c-d), (r+d, c-d), (r-d, c+d)]
   
diagonals :: Position -> [Position]
diagonals = diagonalN 1



getTurnMoves :: GameTurn -> Position -> [Move]
getTurnMoves turn@(GameTurn board@(Board lm) player) posSrc = moves where
   moves = if (pieceMatches player piece)
             then map(Move posSrc) (moveOptions board posSrc) else []
   piece = inSpace lm posSrc
   pieceMatches (Player p1) p2 = basicPiece p1 == basicPiece p2

getAllTurnMoves :: GameTurn -> [Move]
getAllTurnMoves turn@(GameTurn board player) = moves where
   moves = concat $ map (getTurnMoves turn) positions
   positions = boardPositions

getGameMoves :: Game -> [Move]
getGameMoves (Game gs _) = getAllTurnMoves gs

preformMove :: Game -> Move -> Game
preformMove game@(Game gs players) move@(Move p1 p2) = newGame where
   newGame = Game newState3 players
   newState1 = (updateTurn gs p1 p2)
   newState2 = if isJump move
                  then clearTurnPos newState1 jumpedPos
                  else newState1
   newState3 = if (canKing movedPiece p2)
                  then(upgradePieceAt newState2 p2)
                  else newState2
   movedPiece = getTurnPieceAt newState2 p2
    
   jumpedPos = getJumpedPosition move

upgradePieceAt :: GameTurn -> Position -> GameTurn
upgradePieceAt gt pos = newState where
   newState = replaceTurnPos gt pos kingedPiece
   kingedPiece = upgradePiece $ getTurnPieceAt gt pos

isJump :: Move -> Bool
isJump (Move (sc,sr) (dc, dr)) = diff > 1 where
   diff = abs $ sr - dr

getJumpedPosition :: Move -> Position
getJumpedPosition m@(Move(sc, sr)(dc, dr)) = mPos where
   mPos = (sc+deltaColumn,sr+deltaRow)
   deltaColumn = if dc >sc then 1 else -1
   deltaRow = if dr > sr then 1 else -1


--Promotes a given piece to a king 
upgradePiece :: PieceType -> PieceType
upgradePiece Red = King Red
upgradePiece Black = King Black
upgradePiece p = p

--Determines if a piece should be kinged at its current position
canKing :: PieceType -> Position -> Bool
canKing (King _) _ = False -- Don't king if already one
canKing Red (_ ,1) = True -- King if a red piece reaches bottom row
canKing Black (_,8) = True -- King if Black piece reaches top row
canKing _ _ = False -- Handles any other situation

--Game Tests
-- I pray to god that these fuckers actually run