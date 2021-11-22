# Final Exam

## Problem 1

You are given the following data type, representing an encyclopedia:

```haskell
type Concept = String
type Definition = String
type Category = String
data Encyclopedia = Entry Concept Definition | List Category [Encyclopedia]
    deriving Show
```

1.  Write a function that counts how many words representing different categories are in an encyclopedia given as a parameter (in the categories names, no distinction is made between uppercase and lowercase letters, for example 'animal' and 'aNiMal' represent the same category; only words that appear in a `List Category [Encyclopedia]` data type in the `Category` position are considered categories).

    Example:

    ```haskell
    enc1 = List "animal" [List "mammal" [Entry "elephant" "this is an elephant", Entry "dog" "this is a dog", Entry "cat" "this is a cat"], Entry "zebra" "zebra is an animal"]

    enc2 = List "Animal" [List "animal" [Entry "Elephant" "this is an elephant", Entry "dog" "this is a dog", Entry "cat" "this is a cat"], Entry "domesticated animal" "definition"]
    ```

    `enc1` has 2 categories ('animal' and 'mammal'), while `enc2` has 1 category ('animal').

2.  Make `Encyclopedia` an instance of the `Eq` class, so that two encyclopedias are considered equal if they contain the same different categories (in the categories names, no distinction is made between uppercase and lowercase letters; only words that appear in a `List Category [Encyclopedia]` data type in the `Category` position are considered categories).

    Example:

    ```haskell
    enc1 == enc2 = False
    ```

3.  You are given the following data type:

    ```haskell
    type Dictionary = [(Concept, Category, Definition)]
    ```

    Write a function that transforms an `Encyclopedia` value into a `Dictionary` value, so that, for a `Concept`, the associated category is the last `Category` that defines that concept (for example, in `enc1` 'elephant' will have the associated category 'mammal' and not 'animal'). For uncategorized concepts, the `Category` value associated with the record will be 'uncategorized'). No additional processing is done on the initial value of `Encyclopedia` type and no restrictions are imposed on the order of triplets from the `Dictionary` value.

    Example:

    ```haskell
    enc3 = List "animal" [List "Animal" [Entry "elephant" "def1"], Entry "zebra" "def2"]

    -- can be transformed into

    [("elephant", "Animal", "def1"), ("zebra", "animal", "def2")]
    ```

## Problem 2

You are given the following data type:

```haskell
data B e = R e Int | B e ::: B e
    deriving Eq
infixr 5 :::
```

1.  Make the `B` data type an instance of the `Foldable` class.

    Example:

    ```haskell
    fTest0 = maximum (R "grade" 2 ::: R "ten" 3 ::: R "at" 5 ::: R "exam" 1) == "ten"
    ```

2.  Let the following class definition:

    ```haskell
    class C where
        cFilter :: (a -> Bool) -> e a -> e (Maybe a)
        fromList :: [a] -> e a
    ```

    Make `B` an instance of `C` class, where:

    -   `cFilter` changes the elements `x` to `Just x` if the predicate is true, otherwise to `Nothing`
    -   `fromList` transforms the list into an element `E` that looks the same as the original list, adding the order index (pay attention to the order!). For the empty list, the function will throw an error.

    Example:

    ```haskell
    cTest0 = cFilter (\x -> length x >= 4) (fromList ["grade", "ten", "at", "exam"])
        ==
        (R (Just "grade") 1 ::: R Nothing 2 ::: R Nothing 3 ::: R (Just "exam") 4)
    ```
