# Lesson 1 - Haskell Basics

To get acquainted with the GHC environment, first we will test a few basic commands in the terminal. So, open a terminal and run `ghci`. This will run the interpreter and show `Prelude>` in the terminal. [Prelude](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html) is the basic Haskell library.

In the GHC interpreter, we can run:

-   Haskell expressions, that will be evaluated when possible

    ```haskell
    Prelude> 2+3
    5
    Prelude> False || True
    True
    Prelude> x
    <interactive>:1:1: error: Variable not in scope: x
    Prelude> x=3
    Prelude> x
    3
    Prelude> y=x+1
    Prelude> y
    4
    Prelude> head [1,2,3]
    1
    Prelude> head "abcd"
    'a'
    Prelude> tail "abcd"
    "bcd"
    ```

    The `head` and `tail` functions are part of the `Prelude` library.

-   commands (preceded by `:`)

    -   `:?` - help
    -   `:q` - quit
    -   `:cd` - change directory
    -   `:t` - type
    -   `:m` - module
    -   `:l` - load
    -   `:r` - reload

    ```haskell
    Prelude> :t True
    True :: Bool
    ```

To load our `Lib.hs` file, we run the following command in the src folder:

```haskell
Prelude> :l Lib.hs
[1 of 1] Compiling Lib              ( Lib.hs, interpreted )
Ok, one module loaded.
```

## Exercises

1. Try to run `double myInt` and see the result.
2. Create a new function named `triple` that takes as parameter a number and returns its triple value.
3. We want to generate all the permutations of a list. Searching Hoogle for `permutations` function, we find it in the `Data.List` library. To use the function, we must import the library: `import Data.List`.

    ```haskell
    Prelude> :t permutations
    <interactive>:1:1: error: Variable not in scope: permutations

    Prelude> import Data.List

    Prelude Data.List> :t permutations
    permutations :: [a] -> [[a]]

    Prelude Data.List> permutations [1,2,3]
    [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]

    Prelude Data.List> permutations "abc"
    ["abc","bac","cba","bca","cab","acb"]
    ```

    We can remove the imported library using `:m - Data.List`.

    To use the function in our file, we must add the import. All the imports must be at the beginning of the file, but after exporting the module (if there is one).

    Now evaluate `permutations [1..myInt]`. This will generate all the permutations of the list containing all the integer numbers from 1 to `myInt`, so the list has a very big dimension. We can see that we can use very large numbers, but the evaluation will take a lot of time. We can stop the evaluation by entering `Ctrl+C`.

    Search for the `subsequences` function in `Data.List` and understand what it does by creating some examples.

4. Indentation is very important in Haskell. In the `Lib.hs` file, try adding some spaces in the `double` definition and then reload the file. What can you see?

    ```haskell
    double :: Integer -> Integer
       double x = x+x
    ```

    The best practice is to use spaces instead of tabs.

    Now, let's define the `maxi` function.

    ```haskell
    maxi :: Integer -> Integer -> Integer
    maxi x y = if x > y then x else y
    ```

    The indented version of this function is:

    ```haskell
    maxi :: Integer -> Integer -> Integer
    maxi x y = if x > y
                    then x
                    else y
    ```

    Now, write a function that returns the maximum value of 3 values. Of course, one version is

    ```haskell
    maxi3 :: Integer -> Integer -> Integer -> Integer
    maxi3 x y z = maxi x (maxi y z)
    ```

    Write this function without using the `maxi` function, but using `if` and indentation.

    We can write the `maxi3` function using `let..in` syntax:

    ```haskell
    maxi3'' :: Integer -> Integer -> Integer -> Integer
    maxi3'' x y z =
        let
            u = maxi x y
        in
            maxi u z
    ```

    **IMPORTANT!** The `let..in` expression creates a local scope.

    Write a function named `maxi4` using the `let..in` expression and indentation.

    Write a function that tests the `maxi4` function so that the result is `>=` than each of the 4 parameters.

5. In Haskell, there are basic data types (`Integer`, `Bool`, `Char`) and new data types can be created using `[]`.

    `[a]` is the list of objects of the data type `a`. For example, `String` is the same data type as `[Char]`.

    The `Bool` data type is defined in Haskell as

    ```haskell
    data Bool = False | True
    ```

    In this definition, `Bool` is a type constructor, while `True` and `False` are data constructors.

6. Let's implement the Rock Paper Scissors game.

    ```haskell
    data Choice
        = Rock
        | Paper
        | Scissors
        deriving (Eq, Show)
    ```

    `deriving (Eq, Show)` means that, for this data type, there is the natural equality relationship (`Eq`) and that they can be shown as strings (`Show`).

    Define a new data type `Result` with the values `Victory`, `Defeat` and `Draw`. Then write a function named `game :: Choice -> Choice -> Result` that returns `Victory` if the first argument wins, `Defeat` if the first argument loses and `Draw` if the arguments are equal.
