# Lesson 9 - Advanced collection types

In this lesson we will practice manipulating lists and data types
by implementing several collections of the key-value associative table type.

These collections must have the following features:

-   creation of an empty collection
-   creation of a collection with one element
-   inserting / updating an element in a collection
-   searching an element in a collection
-   deleting (or marking as deleted) an element in a collection
-   obtaining the list of the keys
-   obtaining the list of the values
-   obtaining the list of the elements

```haskell
import Prelude hiding (lookup)
import qualified Data.List as List
```

```haskell
class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert
        :: Ord key
        => key -> value -> c key value -> c key value
    lookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    values :: c key value -> [value]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value
```

## Exercises

1. Add default definitions (functions depending on the other functions) for `keys`, `values` and `fromList`.

2. Let `PairList` be the type of lists of key-value pairs.

    ```haskell
    newtype PairList k v
        = PairList { getPairList :: [(k, v)] }
    ```

    Make `PairList` an instance of the `Collection` class.

3. Let `SearchTree` be the type of a binary search tree.

    ```haskell
    data SearchTree key value
        = Empty
        | Node
            (SearchTree key value) -- elements with lower key
            key -- element key
            (Maybe value) -- element value
            (SearchTree key value) -- elements with higher key
    ```

    Notice that the type of the values is `Maybe value`. We do this to make the delete operation much faster, by simply marking the element as deleted. A deleted element will be `Nothing`.

    Make `SearchTree` an instance of the `Collection` class.

4. Let `Element` be a data type for the elements in a B-Tree (extended with `OverLimit`, which represent all the elements that are bigger than the biggest element in the tree).

    ```haskell
    data Element k v
        = Element k (Maybe v)
        | OverLimit
    ```

    a) Make `Element` an instance of the `Eq` class, such that the constructors of `Element` are equal whenever the keys are equal.

    b) Make `Element` an instance of the `Ord` class, such that, when comparing two `Element` constructors, they are equal if the keys are equal and `OverLimit` is the largest element.

    c) Make `Element` an instance of the `Show` class, such that `Element k (Just v)` will be printed as the `(k, v)` pair, `Element k Nothing` will be `k` and `OverLimit` is the empty list.

5. Let `BTree` be the data type of a B-tree.

    ```haskell
    data BTree key value
        = BEmpty
        | BNode [(BTree key value, Element key value)]
    ```

    Make `BTree` an instance of the `Collection` class.

    **Notes:**

-   Searching for a key / the position of a key `k`:

    -   we search through the `(subtree, element)` pair list
    -   when we found an element with the key `>= k`, we stop
    -   otherwise, we repeat the process in the corresponding subtree

-   Inserting a new element:

    -   we search the tree as defined above
    -   if we found the key, we replace its value with the new one
    -   otherwise, a new `singleton` tree is created and it is marked
    -   when going back, if the processed subtree is marked, then:

        -   it will be like `ls-(k, v)-rs` (single element node)
        -   we break the current pair `(a, e)` in `(ls, (k, v)) (rs, e)`
        -   if the number of elements is `2*order+1`, we break it in half; it becomes `ls-(k, v)-rs` and it is marked
