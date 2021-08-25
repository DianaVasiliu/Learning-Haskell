import Data.List ()

matrix :: Num a => [[a]]
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

correct :: [[a]] -> Bool
correct [] = True
correct [_] = True
correct (x:y:xs) = (length x == length y) && correct (y:xs)

test1 :: Bool
test1 = correct [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

test2 :: Bool
test2 = correct [[1, 2, 3], [4, 5, 6], [7, 9]]

el :: [[a]] -> Int -> Int -> a
el [] _ _ = error "Empty matrix"
el m row col 
    | correct m 
    && row < length m 
    && col < length (head m)
    = (m !! row) !! col
    | otherwise = error "Wrong index or incorrect matrix"

transform :: [[a]] -> [(a, Int, Int)]
transform m     
    | correct m = [(x, r, c) | 
                    r <- [0..rows], 
                    c <- [0..cols], 
                    let x = m !! r !! c]
    | otherwise = error "Incorrect matrix"
    where
        rows = length m - 1
        cols = length (head m) - 1
