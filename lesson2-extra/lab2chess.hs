-- Informatics 1 - Functional Programming
-- Lab week tutorial part II
--
--

import Data.Char ( isDigit, toLower, digitToInt )
import PicturesSVG
    ( Picture(Empty),
        above,
        beside,
        king,
        queen,
        bishop,
        knight,
        rook,
        pawn,
        clear,
        render )

-- Exercise 1
-- write the correct type and the definition for
-- isFENChar
isFENChar :: Char -> Bool
isFENChar c 
    | toLower c `elem` "rnbqkp" = True
    | isDigit c                 = True
    | c == '/'                  = True
    | otherwise                 = False


-- Exercise 2.a
-- write a recursive definition for
besideList :: [Picture] -> Picture
besideList = foldr beside Empty

-- Exercise 2.c
-- write the correct type and the definition for
-- toClear
toClear :: Int -> Picture
toClear n 
    | n <= 0 = Empty
    | otherwise = besideList (replicate n clear)


-- Exercise 3
-- write the correct type and the definition for
-- fenCharToPicture
fenCharToPicture :: Char -> Picture
fenCharToPicture c
    | toLower c `elem` "rnbqkp" =
        case toLower c of
            'r' -> rook
            'n' -> knight
            'b' -> bishop
            'q' -> queen
            'k' -> king
            'p' -> pawn
    | isDigit c = toClear $ digitToInt c
    | otherwise = Empty
    

-- Exercise 4
-- write the correct type and the definition for
-- isFENRow
isFENRow :: String -> Bool
isFENRow str = and [isFENChar s | s <- str]


-- Exercise 6.a
-- write a recursive definition for
fenCharsToPictures :: String -> [Picture]
fenCharsToPictures "" = [Empty]
fenCharsToPictures s
    | isFENRow s = createPictures s
    | otherwise = [Empty]
    where
        createPictures (x:xs) = fenCharToPicture x : fenCharsToPictures xs

-- Exercise 6.b
-- write the correct type and the definition of
-- fenRowToPicture
fenRowToPicture :: String -> Picture
fenRowToPicture s = besideList (fenCharsToPictures s)


-- Exercise 7
-- write the correct type and the definition of
-- mySplitOn
mySplitOn :: Char -> String -> [String]
mySplitOn x = foldr (
                \c (s:ss) -> 
                    if c == x 
                        then "" : s : ss 
                        else (c : s) : ss
                ) [""]


-- Exercise 8.a
-- write a recursive definition for
fenRowsToPictures :: [String] -> [Picture]
fenRowsToPictures = map fenRowToPicture

-- Exercise 8.b
-- write the correct type and the definition of
-- aboveList
aboveList :: [Picture] -> Picture
aboveList = foldr above Empty

-- Exercise 8.c
-- write the correct type and the definition of
-- fenToPicture
fenToPicture :: [String] -> Picture
fenToPicture = aboveList . fenRowsToPictures


-- Exercise 9
-- write the correct type and the definition of
-- chessBoard
chessBoard :: String -> Picture
chessBoard str = fenToPicture (mySplitOn '/' str)

renderChessBoard :: IO ()
renderChessBoard = render $ chessBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

-- Exercise 10
-- write the correct type and definition of
-- allowedMoves
allowedMoves :: Char -> (Char, Int) -> [(Char, Int)]
allowedMoves piece pos
    | toLower piece `notElem` "rnbqkp"      = error "Invalid Chess Piece"
    | toLower c < 'a' || toLower c > 'h'    = error "Invalid Column"
    | r < 1 || r > 8                        = error "Invalid Row"
    | otherwise                             = case piece of 
                                                'r' -> rookMoves pos
                                                'n' -> knightMoves pos
                                                'b' -> bishopMoves pos
                                                'q' -> queenMoves pos
                                                'k' -> kingMoves pos
                                                'p' -> pawnMoves pos 'w' -- can be changed to 'b' or transformed into a parameter
    where 
        (c, r) = pos

rookMoves :: (Char, Int) -> [(Char, Int)]
rookMoves (c, r) = allOnColumn ++ allOnRow
    where
        allOnColumn = [(c, newRow) | newRow <- [1..8], newRow /= r]
        allOnRow = [(newColumn, r) | newColumn <- ['a'..'h'], newColumn /= c]

knightMoves :: (Char, Int) -> [(Char, Int)]
knightMoves (c, r) = [(newC, newR) | (newC, newR) <- 
                        [
                            (pred $ pred c, r+1), 
                            (pred $ pred c, r-1),
                            (pred c, r+2),
                            (pred c, r-2),
                            (succ c, r+2),
                            (succ c, r-2),
                            (succ $ succ c, r+1),
                            (succ $ succ c, r-1)                        
                        ],
                    newC >= 'a', newC <= 'h', newR >= 1, newR <= 8]

bishopMoves :: (Char, Int) -> [(Char, Int)]
bishopMoves (c, r) = diagTopLeft ++ diagBottomLeft ++ diagTopRight ++ diagBottomRight
    where
        diag :: Char -> Int -> Int -> Int -> [(Char, Int)]
        diag c r rowDir colDir 
            | newC >= 'a' && newC <= 'h' && newR >= 1 && newR <= 8 
                = (newC, newR) : diag newC newR rowDir colDir
            | otherwise = []
            where
                newC = if colDir == 1 
                            then succ c
                            else pred c
                newR = r + rowDir

        diagTopLeft = diag c r 1 (-1)
        diagBottomLeft = diag c r (-1) (-1)
        diagTopRight = diag c r 1 1
        diagBottomRight = diag c r (-1) 1

queenMoves :: (Char, Int) -> [(Char, Int)]
queenMoves pos = rookMoves pos ++ bishopMoves pos

kingMoves :: (Char, Int) -> [(Char, Int)]
kingMoves (c, r) = left ++ sameCol ++ right
    where
        left = 
            if c > 'a' 
                then [(pred c, newRow) | newRow <- [r-1, r, r+1], newRow >= 1, newRow <= 8]
                else []
        sameCol = [(c, newRow) | newRow <- [r-1, r+1], newRow >= 1, newRow <= 8]
        right = 
            if c < 'h'
                then [(succ c, newRow) | newRow <- [r-1, r, r+1], newRow >= 1, newRow <= 8]
                else []
        
pawnMoves :: (Char, Int) -> Char -> [(Char, Int)]
pawnMoves (c, r) color
    | color == 'b'
    = if r == 7 
        then
            [(c, r - 1), (c, r - 2)]
        else
            [(c, r - 1) | r - 1 >= 1]
    | r == 2 = [(c, r + 1), (c, r + 2)]
    | otherwise = [(c, r + 1) | r + 1 <= 8]
