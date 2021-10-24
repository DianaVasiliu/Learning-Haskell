import Data.List
import Data.Char

type Concept  = String
type Definition = String
type Category = String

data Encyclopedia 
    = Entry Concept Definition  
    | List Category [Encyclopedia]
    deriving Show


enc1 = List "animal" [List "mammal" [Entry "elephant" "this is an elephant", Entry "dog" "this is a dog", Entry "cat" "this is a cat"], Entry "zebra" "zebra is an animal"]

enc2 = List "Animal"[List "animal" [Entry "elephant" "this is an elephant", Entry "dog" "this is a dog", Entry "cat" "this is a cat"], Entry "domesticated animal" "Definition"]

enc3 :: Encyclopedia
enc3 = List "creature" [
            List "Animal" [List "mammal" [Entry "dog" "", Entry "cat" ""],
                            List "bird" [Entry "parrot" ""], Entry "dove" ""], 
            List "Plant" [List "vegetable" [Entry "cucumber" ""],
                            List "fruit" [Entry "apple" ""]]
       ]

enc4 :: Encyclopedia
enc4 = List "Plant" [
            List "flowers" [List "purple" [Entry "iris" "", List "with spots" [Entry "something" ""]],
                           List "red" [Entry "rose" "", List "with spots" [Entry "tulip" ""]]],
            List "vegetable-fruit" [List "vegetable" [Entry "tomato" "", Entry "cucumber" ""],
                                   List "fruit" [Entry "apple" "", Entry "pear" ""]],
            List "vegetable" [Entry "tomato" "", Entry "cucumber" ""],
            List "fruit" [Entry "apple" "", Entry "pear" ""],
            Entry "other Plants" ""                    
      ]


categories :: Encyclopedia -> [Category]
categories (Entry _ _) = []
categories (List c lst) = c : concat (map categories lst)

differentCategories :: [Category] -> [Category]
differentCategories lst = nub (map (map toLower) lst)

count :: Encyclopedia -> Int
count (Entry _ _) = 0
count enc@(List c lst) = length (differentCategories (categories enc))

test6 :: Bool
test6 = count enc3 == 7
test7 :: Bool
test7 = count enc4 == 8

instance Eq Encyclopedia where
    Entry _ _ == Entry _ _ = False
    Entry _ _ == List _ _ = False
    List _ _ == Entry _ _ = False
    List c1 l1 == List c2 l2 = 
        let
            cat1 = differentCategories (categories (List c1 l1))
            cat2 = differentCategories (categories (List c2 l2))
            sortedC1 = sort cat1
            sortedC2 = sort cat2
        in sortedC1 == sortedC2

enc5 :: Encyclopedia
enc5 = List "creature" [
            List "Plant" [List "fruit" [Entry "apple" ""],
                            List "vegetable" [Entry "cucumber" ""]],
            List "AniMAL" [List "mammal" [Entry "dog" "", Entry "cat" ""],
                            List "bird" [Entry "parrot" ""], Entry "dove" ""]           
        ]

test8 :: Bool
test8 = (enc3 == enc5) == True

test9 :: Bool
test9 = (enc4 == enc5) == False 


type Dictionary = [(Concept, Category, Definition)]



data B e 
    = R e Int 
    | B e ::: B e
    deriving Eq
infixr 5 :::

instance (Show e) => Show (B e) where
    show (R e i) = "R " ++ show e ++ " " ++ show i
    show (a ::: b) = show a ++ " ::: " ++ show b

instance Foldable B where
    foldMap f (R e i) = f e
    foldMap f (a ::: b) = foldMap f a <> foldMap f b

fTest0 = maximum (R "grade" 2 ::: R "ten" 3 ::: R "at" 5 ::: R "exam" 1) == "ten"

fTest1 :: Bool
fTest1 = maximum (R "abc" 10 ::: R "pqr" 12 ::: R "xyz" 1) == "xyz"

fTest2 :: Bool
fTest2 = maximum (R 'c' 10 ::: R 'z' 12 ::: R '-' 1) == 'z'

fTest3 :: Bool
fTest3 = minimum (R (Just 5) 10 ::: R (Just 12) 12 ::: R (Just 10) 1) == Just 5


class C e where
    cFilter :: (a -> Bool) -> e a -> e (Maybe a)
    fromList :: [a] -> e a

instance C B where
    cFilter p (a ::: b) = cFilter p a ::: cFilter p b
    cFilter p (R e i)
        | p e = R (Just e) i
        | otherwise = R Nothing i

    fromList [] = error "Empty list!"
    fromList lst = concatB $ map makeB zipList
        where
            zipList = zip lst [1..]

            makeB (e, i) = R e i

            concatB (x:xs) 
                | not $ null xs = x ::: concatB xs
                | otherwise = x

cTest0 = 
    cFilter 
    (\x -> length x >= 4) 
    (fromList ["grade", "ten", "at", "exam"]) 
    ==
    (R (Just "grade") 1 ::: R Nothing 2 ::: R Nothing 3 ::: R (Just "exam") 4)
