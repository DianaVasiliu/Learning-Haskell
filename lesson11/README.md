# Lesson 11 - Foldable, Functor,

```haskell
import Data.Monoid
import Data.Semigroup (Max (..), Min (..))
import Data.Foldable (foldMap, foldr)
import Data.Char (isUpper)
```

## Exercises - Foldable

1.  Implement the following functions using `foldMap` and/or `foldr` in the `Foldable` class, then test them with different types that have a `Foldable` instance.

    ```haskell
    elem :: (Foldable t, Eq a) => a -> t a -> Bool
    elem = undefined

    null :: (Foldable t) => t a -> Bool
    null = undefined

    length :: (Foldable t) => t a -> Int
    length = undefined

    toList :: (Foldable t) => t a -> [a]
    toList = undefined

    fold :: (Foldable t, Monoid m) => t m -> m
    fold = undefined -- Hint: use foldMap
    ```

2.  Write `Foldable` instances for the following types:

    ```haskell
    data Constant a b = Constant b

    data Two a b = Two a b

    data Three a b c = Three a b c

    data Three' a b = Three' a b b

    data Four' a b = Four' a b b b

    data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    ```

3.  Write a filter function for `Foldable`.

    ```haskell
    filterF
        :: (Applicative f
        , Foldable t
        , Monoid (f a)
        )
        => (a -> Bool) -> t a -> f a
    filterF = undefined -- Hint: use foldMap
    ```

    Here are five tests for the filter function.

    ```haskell
    unit_testFilterF1 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == "HEIBWF"
    unit_testFilterF2 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == First (Just 'H')
    unit_testFilterF3 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == Min 'B'
    unit_testFilterF4 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == Max 'W'
    unit_testFilterF5 = filterF Data.Char.isUpper "tHE quIck BroWn Fox" == Last (Just 'F')
    ```

## Exercises - Functor

Write instances for the `Functor` class for the following data types:

```haskell
newtype Identity a = Identity a

data Pair a = Pair a a

-- Two data type defined above
-- Three data type defined above
-- Three' data type defined above

data Four a b c d = Four a b c d

data Four'' a b = Four'' a a a b

-- Constant data type defined above

data Quant a b = Finance | Desk a | Bloor b

data K a b = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
-- for Flip, you don't have to write an instance

instance Functor (Flip K a) where
    fmap = undefined

data LiftItOut f a = LiftItOut (f a)

data Parappa f g a = DaWrappa (f a) (g a)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

data Notorious g o a t = Notorious (g o) (g a) (g t)

-- GoatLord data type defined above

data TalkToMe a = Halt | Print String a | Read (String -> a)
```
