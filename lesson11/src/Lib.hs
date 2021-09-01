module Lib where

import Data.Monoid
import Data.Semigroup (Max (..), Min (..))
import Data.Foldable (foldMap, foldr)
import Data.Char (isUpper)

-- elem 2 [3,2,5] 
-- => [Any False, Any True, Any False] 
-- => Any True 
-- => True

-- TESTS:
-- 5 `Lib.elem` [1,4,5] 
-- 5 `Lib.elem` (Right 5)
-- 5 `Lib.elem` (Left 3)
-- 5 `Lib.elem` (Just 5)
-- 5 `Lib.elem` Nothing

-- newtype Any = Any { getAny :: Bool }
-- Semigroup instance:
--   Any b1 <> Any b2 == Any (b1 || b2)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x collection = 
    getAny
    $ foldMap isAnyEqual collection
    where
        -- isAnyEqual :: a -> Any
        isAnyEqual y = Any (x == y)

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length collection = 
    getSum
    $ foldMap (Sum . const 1) collection


toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id


data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord where
    foldMap _ NoGoat = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats g1 g2 g3) = 
        foldMap f g1 <> foldMap f g2 <> foldMap f g3

filterF
    :: (Applicative f
    , Foldable t
    , Monoid (f a)
    )
    => (a -> Bool) -> t a -> f a
filterF f = foldMap select
    where
        select a
            | f a = pure a
            | otherwise = mempty

unit_testFilterF1 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == "HEIBWF"
unit_testFilterF2 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == First (Just 'H')
unit_testFilterF3 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == Min 'B'
unit_testFilterF4 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == Max 'W'
unit_testFilterF5 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == Last (Just 'F')


newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- Two data type defined above
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- Three data type defined above
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

-- Three' data type defined above
instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)

-- Constant data type defined above
instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data K a b = K a
instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) 
    deriving (Eq, Show)

-- instance Functor (Flip K a) where
--     fmap f (Flip (K b)) = Flip (K (f b))

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- GoatLord data type defined above
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read fa) = Read (f . fa)
