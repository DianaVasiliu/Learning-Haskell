import Data.List ()
import Data.Char

--------------------------
---- ENCRYPTION ----------
--------------------------

rotate :: Int -> [Char] -> [Char]
rotate n str 
    | n < 0 || n > length str = error "Invalid rotation"
    | otherwise = end ++ start
    where
        pair = splitAt n str
        start = fst pair
        end = snd pair

test :: Bool
test = rotate 3 "ABCDEFGHIJKLMNOPQRSTUVWXYZ" == "DEFGHIJKLMNOPQRSTUVWXYZABC"

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
    where 
        l = length str
        m = if l == 0 then 0 else k `mod` l

makeKey :: Int -> [(Char, Char)]
makeKey n = 
    let alphabet = ['A'..'Z']
    in zip alphabet (rotate n alphabet)

lookUp :: Char -> [(Char, Char)] -> Char
lookUp chr [] = chr
lookUp chr (x:xs) 
    | chr == fst x  = snd x
    | otherwise     = lookUp chr xs

encipher :: Int -> Char -> Char
encipher n chr = lookUp chr (makeKey n)

normalize :: String -> String
normalize "" = ""
normalize (s:xs) 
    | not . isAlphaNum $ s = normalize xs
    | otherwise = toUpper s : normalize xs

encipherStr :: Int -> String -> String
encipherStr n str = [encipher n s | s <- normalize str]

--------------------------
---- DECRYPTION ----------
--------------------------

reverseKey :: [(Char, Char)] -> [(Char, Char)] 
reverseKey lst = [(y, x) | (x, y) <- lst]

decipher :: Int -> Char -> Char
decipher n chr = lookUp chr (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr _ "" = ""
decipherStr n (x:xs) 
    | isAlphaNum x || x == ' '
    = decipher n x : decipherStr n xs
    | otherwise = decipherStr n xs

-----------------------------------
---- ABSTRACT DATA TYPES ----------
-----------------------------------

-- FRUITS

data Fruit 
    = Apple String Bool
    | Orange String Int

jonathanWithoutWorms :: Fruit
jonathanWithoutWorms = Apple "Jonathan" False

goldenWithWorms :: Fruit
goldenWithWorms = Apple "Golden Delicious" True

sicilianOrange10 :: Fruit
sicilianOrange10 = Orange "Sanguinello" 10

fruitList = [
    Apple "Jonathan" False,
    Orange "Sanguinello" 10,
    Orange "Valencia" 22,
    Apple "Golden Delicious" True,
    Orange "Sanguinello" 15,
    Orange "Moro" 12,
    Orange "Tarocco" 3,
    Orange "Moro" 12,
    Orange "Valencia" 2,
    Apple "Golden Delicious" False,
    Apple "Golden" False,
    Apple "Golden" True
    ]

isSicilianOrange :: Fruit -> Bool
isSicilianOrange (Orange variety _) = 
    variety `elem` ["Tarocco", "Moro", "Sanguinello"]
isSicilianOrange _ = False

test_isSicilianOrange1 :: Bool
test_isSicilianOrange1 =
    isSicilianOrange (Orange "Moro" 12)

test_isSicilianOrange2 :: Bool
test_isSicilianOrange2 =
    not $ isSicilianOrange (Apple "Jonathan" True)

getSlices :: Fruit -> Int
getSlices (Orange _ num) = num
getSlices _ = 0

numSicilianSlices :: [Fruit] -> Int
numSicilianSlices [] = 0
numSicilianSlices (x:xs) 
    | isSicilianOrange x = getSlices x + numSicilianSlices xs
    | otherwise = numSicilianSlices xs

numSicilianSlices' :: [Fruit] -> Int
numSicilianSlices' l = sum (map getSlices (filter isSicilianOrange l))

test_numSicilianSlices :: Bool
test_numSicilianSlices = numSicilianSlices fruitList == 52

numWormApples :: [Fruit] -> Int
numWormApples l = sum [hasWorm x | x <- l]
    where
        hasWorm :: Fruit -> Int
        hasWorm (Apple _ x)
            | x         = 1
            | otherwise = 0
        hasWorm _ = 0

test_numWormApples :: Bool
test_numWormApples = numWormApples fruitList == 2

-- Matrices 

data Row = R [Int]
    deriving Show

data Matrix = M [Row]

fromMatrix :: Matrix -> [Row]
fromMatrix (M m) = m

fromRow :: Row -> [Int]
fromRow (R r) = r

verify :: Matrix -> Int -> Bool
verify mat n = and [s == n | 
                        row <- fromMatrix mat, 
                        let r = fromRow row,
                        let s = sum r]

verifyFold :: Matrix -> Int -> Bool
verifyFold mat n = foldr (&&) True lst
    where
        lst = [s == n | 
                    row <- fromMatrix mat, 
                    let r = fromRow row,
                    let s = sum r]

instance Show Matrix where
    show (M mat) = showRows mat
        where
            showRows [] = ""
            showRows (x : xs) = showItems (fromRow x) ++ "\n" ++ showRows xs

            showItems [] = "" 
            showItems (x : xs) = show x ++ " " ++ showItems xs

onlyPositiveN :: Matrix -> Int -> Bool
onlyPositiveN (M mat) n = 
    and [x > 0 | 
            row <- mat, 
            length (fromRow row) == n, 
            x <- fromRow row]
