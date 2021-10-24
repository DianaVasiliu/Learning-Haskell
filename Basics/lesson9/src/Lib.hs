import qualified Data.List as List
import           Prelude   hiding (lookup)


class Collection c where
    empty :: c key value

    singleton :: key -> value -> c key value

    insert
        :: Ord key
        => key -> value -> c key value -> c key value

    lookup :: Ord key => key -> c key value -> Maybe value

    delete :: Ord key => key -> c key value -> c key value

    keys :: c key value -> [key]
    keys = map fst . toList

    values :: c key value -> [value]
    values = map snd . toList

    toList :: c key value -> [(key, value)]

    fromList :: Ord key => [(key,value)] -> c key value
    fromList = foldr (uncurry insert) empty

newtype PairList k v
    = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []

    singleton k v = PairList [(k, v)]

    insert k v lst = PairList (getPairList lst ++ [(k, v)])

    lookup k lst = find k list
        where
            list = getPairList lst
            find _ [] = Nothing
            find key ((x, y) : xs) 
                | key == x = Just y
                | otherwise = find key xs

    delete k lst = PairList (del k list)
        where
            list = getPairList lst
            del _ [] = []
            del key ((x, y) : xs)
                | key == x = del key xs
                | otherwise = (x, y) : del key xs

    toList lst = getPairList lst

data SearchTree key value
    = Empty
    | Node
        (SearchTree key value) -- elements with lower key
        key -- element key
        (Maybe value) -- element value
        (SearchTree key value) -- elements with higher key

instance Collection SearchTree where
    empty = Empty

    singleton k v = Node Empty k (Just v) Empty

    insert k v Empty = singleton k v
    insert k v (Node l key value r)
        | k < key   = Node (insert k v l) key value r
        | k > key   = Node l key value (insert k v r)
        | otherwise = Node l key (Just v) r

    lookup _ Empty = Nothing
    lookup k (Node l key value r)
        | k < key = lookup k l
        | k > key = lookup k r
        | otherwise = value

    delete _ Empty = Empty
    delete k (Node l key value r)
        | k < key   = Node (delete k l) key value r
        | k > key   = Node l key value (delete k r)
        | otherwise = Node l key Nothing r

    toList Empty = []
    toList (Node l key (Just value) r) = toList l ++ [(key, value)] ++ toList r
    toList (Node l key Nothing r) = toList l ++ toList r

data Element k v
    = Element k (Maybe v)
    | OverLimit

instance Eq k => Eq (Element k v) where
    Element k1 _ == Element k2 _ = k1 == k2
    Element _ _ == OverLimit = False
    OverLimit == Element _ _ = False
    OverLimit == OverLimit = True

instance Ord k => Ord (Element k v) where
    Element k1 _ <= Element k2 _    = k1 <= k1
    Element k v <= OverLimit        = True
    OverLimit <= Element k v        = False

instance (Show k, Show v) => Show (Element k v) where
    show OverLimit = "[]"
    show (Element k (Just v)) = "(" ++ show k ++ ", " ++ show v ++ ")"
    show (Element k Nothing) = show k

order :: Int
order = 1

data BTree key value
    = BEmpty
    | BNode [(BTree key value, Element key value)]

instance Collection BTree where
    empty = BEmpty

    singleton k v = BNode [(BEmpty, Element k (Just v)), (BEmpty, OverLimit)]

    lookup k BEmpty = Nothing
    lookup k (BNode lst) = find lst
        where
            find ((lessThan_key, OverLimit) : l) = lookup k lessThan_key
            find ((lessThan_key, Element key value) : l)
                | k == key = value
                | k < key = lookup k lessThan_key
                | k > key = find l
    
    delete _ BEmpty = BEmpty
    delete k (BNode lst) = BNode (del lst)
        where
            del ((lessThan_key, OverLimit) : l) = (delete k lessThan_key, OverLimit) : l
            del ((lessThan_key, Element key value) : l)
                | k == key = (lessThan_key, Element key Nothing) : l
                | k < key  = (delete k lessThan_key, Element key value) : l
                | k > key  = (lessThan_key, Element key value) : del l

    toList BEmpty = []
    toList (BNode lst) = tol lst
        where
            tol ((lessThan_key, OverLimit) : _) = toList lessThan_key
            tol ((lessThan_key, e) : l) = toList lessThan_key ++ elemToList e ++ tol l
            elemToList (Element _ Nothing) = []
            elemToList (Element key (Just value)) = [(key, value)]

    insert k v tree = fst (ins k v tree)
        where
            ins k v BEmpty = (singleton k v, True)
            ins k v (BNode lst) = balance (go lst)
            
            go ((lessThan_key, OverLimit) : l) =
                let (lessThan_key_2, e) = ins k v lessThan_key
                in
                    if e 
                        then 
                            let (BNode [(lessThan_key_3, e2), (greatherThan_key_3, OverLimit)])
                                    = lessThan_key_2
                            in [(lessThan_key_3, e2), (greatherThan_key_3, OverLimit)]
                        else
                            (lessThan_key_2, OverLimit) : l
            go ((lessThan_key, Element key value) : l)
                | k == key = (lessThan_key, Element key (Just v)) : l
                | k < key  =
                    let (lessThan_key_2, e) = ins k v lessThan_key
                    in 
                        if e
                            then 
                                let (BNode [(lessThan_key_3, e2), (greatherThan_key_3, OverLimit)])
                                        = lessThan_key_2
                                in (lessThan_key_3, e2) : (greatherThan_key_3, Element key value) : l
                            else
                                (lessThan_key_2, Element key value) : l
                | k > key  = (lessThan_key, Element key value) : go l
            
            balance nodes 
                | lnode <= 2 * order + 1 = (BNode nodes, False)
                | otherwise = (BNode [(BNode (little ++ [(medium, OverLimit)]), middle), (BNode big, OverLimit)], True)
                where
                    lnode = length nodes
                    little = take order nodes
                    ((medium, middle) : big) = drop order nodes
