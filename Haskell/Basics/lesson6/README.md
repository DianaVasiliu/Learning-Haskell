# Lesson 6 - Encryption and Decryption, Abstract data types

## Encryption and Decryption

In cryptography, Caesar's cipher, also called shifting cipher, Caesar's code, or Caesar's displacement, is one of the simplest and best-known encryption techniques. It is a type of substitution cipher, in which each letter of the initial text is replaced by a letter that is in the alphabet at a fixed distance from the replaced one. For example, with a shift of five positions in the English alphabet, A is replaced by F, B becomes G and so on. This method is named after Julius Caesar, who used it to communicate with his generals. The idea is simple: take a message we want to encrypt and move all the letters with a certain value between 0 and 26. For example, if we want to encrypt the sentence "THIS IS A BIG SECRET" shifting 5 letters, the resulted string will be “YMNX NX F GNL XJHWJY”. Next we will implement a variant of this cipher.

### Encoding a message

Character-to-character encryption can be represented by a key, using a list of pairs. Each pair in the list indicates for a letter what its encoding is. For example a digit for the letters A-E can be given by the list [('A', 'C'), ('B', 'D'), ('C', 'E'), ('D', 'A'), ('E','B')].

1.  We can rotate a list, taking a part from the beginning and adding it to the end.

    Example:

    ```haskell
    *Main> rotate 3 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "DEFGHIJKLMNOPQRSTUVWXYZABC"
    ```

    Write the function `rotate :: Int -> [Char] -> [Char]`. For a number `n`, `0 < n < length list`, the function will rotate the list by `n` elements. The function must return an error if `n` is negative or if `n` is too big.

2.  Analyze the function below. What does it do? How does it avoid throwing an error?

    ```haskell
    prop_rotate :: Int -> String -> Bool
    prop_rotate k str = rotate (l + 1 - m) (rotate m str') == str'
        where
            str' = "ab" ++ str
            l = length str + 1
            m = 1 + k `mod` l
    ```

3.  Using the `rotate` function, write a function `makeKey :: Int -> [(Char, Char)]` that returns the encryption key with a given shifting for all the uppercase letters in the English alphabet.

    Example:

    ```haskell
    *Main> makeKey 5
    [('A','F'),('B','G'),('C','H'),('D','I'),('E','J'),('F','K'),('G','L'),('H','M'),('I','N'),('J','O'),('K','P'),('L','Q'),('M','R'),('N','S'),('O','T'),('P','U'),('Q','V'),('R','W'),('S','X'),('T','Y'),('U','Z'),('V','A'),('W','B'),('X','C'),('Y','D'),('Z','E')]
    ```

4.  Write a function `lookUp :: Char -> [(Char, Char)] -> Char` that searches a list of pairs of characters for a specific character (on the first component) and returns the second component of the pair. If there is no such pair, then the function will return the parameter character.

    Examples:

    ```haskell
    *Main> lookUp 'B' [('A', 'F'), ('B', 'G'), ('C', 'H')]
    'G'
    *Main> lookUp '9' [('A', 'X'), ('B', 'Y'), ('C', 'Z')]
    '9'
    ```

5.  Write a function `encipher :: Int -> Char -> Char` that encrypts a character using the key given by a given displacement as a parameter.

    Examples:

    ```haskell
    *Main> encipher 5 'C'
    'H'
    *Main> encipher 7 'Q'
    'X'
    ```

6.  In order to be encrypted, the text must not contain punctuation marks and must be written in capital letters. Write a function `normalize :: String -> String` that normalizes a string, turning lowercase to uppercase and removing all characters that are not letters or numbers.

    Example:

    ```haskell
    *Main> normalize "July 4th!"
    "JULY4TH"
    ```

7.  Write a function `encipherStr :: Int -> String -> String` that normalizes a string and encrypts it using the functions defined before.

    Example:

    ```haskell
    *Main> encipherStr 5 "July 4th!"
    "OZQD4YM"
    ```

### Decoding a message

8. Write a function `reverseKey :: [(Char, Char)] -> [(Char, Char)]` to revert the encryption key, switching the components of each pair.

    Example:

    ```haskell
    *Main> reverseKey [('A', 'G'), ('B', 'H'), ('C', 'I')]
    [('G','A'),('H','B'),('I','C')]
    ```

9. Write the functions `decipher :: Int -> Char -> Char` and `decipherStr :: Int -> String -> String` to decrypt a character and a string using the key generated by a given shifting value. The function will ignore the spaces and the digits, but will remove the lowercase characters or other characters.

    Example:

    ```haskell
    *Main> decipherStr 5 "OZQD4YM"
    "JULY4TH"
    ```

## Abstract Data Types

### Fruits

We define a new data type:

```haskell
data Fruit
    = Apple String Bool
    | Orange String Int
```

A `Fruit` is either an `Apple` or an `Orange`. We will use a `String` to indicate the variety of Apple or Orange. We will use a `Bool` for an Apple to indicate if the apple has worms and a `Int` for an Orange to indicate the number of slices of an orange.

Example:

```haskell
jonathanWithoutWorms = Apple "Jonathan" False
goldenWithWorms = Apple "Golden Delicious" True
sicilianOrange10 = Orange "Sanguinello" 10
```

```haskell
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
```

## Exercises

1. Write a function that indicates whether a fruit is a Sicilian Orange or not. The Sicilian Oranges are "Tarocco", "Moro" and "Sanguinello".

    ```haskell
    isSicilianOrange :: Fruit -> Bool
    isSicilianOrange = undefined
    ```

    Examples:

    ```haskell
    test_isSicilianOrange1 =
        isSicilianOrange (Orange "Moro" 12) == True
    test_isSicilianOrange2 =
        isSicilianOrange (Apple "Jonathan" True) == False
    ```

2. Write a function that calculates the total number of slices of the Sicilian Oranges in a fruit list.

    ```haskell
    numSicilianSlices :: [Fruit] -> Int
    numSicilianSlices = undefined
    ```

    Example:

    ```haskell
    test_numSicilianSlices = numSicilianSlices fruitList == 52
    ```

3. Write a function that calculates the number of apples in a fruit list that have worms.

    ```haskell
    numWormApples :: [Fruit] -> Int
    numWormApples = undefined
    ```

    Example:

    ```haskell
    test_numWormApples = numWormApples fruitList == 2
    ```

### Matrices

We define two new data types:

```haskell
data Row = R [Int]
    deriving Show
data Matrix = M [Row]
```

## Exercises

1. Write a function that checks if the sum of the elements on each row is equal to a value `n`. Use `foldr`.

    Examples:

    ```haskell
    *Main> verify (M [R [1,2,3], R [4,5], R [2,3,6,8], R [8,5,3]]) 10
    False
    *Main> verify (M [R [2,20,3], R [4,21], R [2,3,6,8,6], R [8,5,3,9]]) 25
    True
    ```

2. Write an instance of the `Show` class for the `Matrix` data type, so that each row is printed on a new line.

    Example:

    ```haskell
    *Main> M [R [1,2,3], R [4,5], R [2,3,6,8], R [8,5,3]]
    1 2 3
    4 5
    2 3 6 8
    8 5 3
    ```

3. Write a function `onlyPositiveN` that takes as parameter an element of type `Matrix` and an integer number `n` and verifies that all the matrix rows of length `n` have only positive values.

    Examples:

    ```haskell
    *Main> onlyPositiveN (M [R [1,2,3], R [4,5], R [2,3,6,8], R [8,5,3]]) 3
    True
    *Main> onlyPositiveN (M [R [1,2,-3], R [4,5], R [2,3,6,8], R [8,5,3]]) 3
    False
    ```
