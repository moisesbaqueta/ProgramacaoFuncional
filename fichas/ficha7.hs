-- Exercício 1
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- a)
calcula :: ExpInt -> Int
calcula x = case x of
            Const x -> x
            Simetrico x -> - (calcula x)
            Mais x y -> calcula x + calcula y
            Menos x y -> calcula x - calcula y
            Mult x y -> calcula x * calcula y

-- b)
infixa :: ExpInt -> String
infixa x = case x of
           Const x -> show x
           Simetrico x -> "-" ++ "(" ++ infixa x ++ ")"
           Mais x y -> "(" ++ infixa x ++ "+" ++ infixa y ++ ")"
           Menos x y -> "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
           Mult x y -> "(" ++ infixa x ++ "*" ++ infixa y ++ ")"

-- c)
posfixa :: ExpInt -> String
posfixa x = case x of
            Const x -> show x
            Simetrico x -> posfixa x ++ "-"
            Mais x y ->  posfixa x ++ " " ++ posfixa y ++ " +"
            Menos x y -> posfixa x ++ " " ++ posfixa y ++ " -"
            Mult x y -> posfixa x ++ " " ++ posfixa y ++ " *"

-- Exercício 2
data RTree a = R a [RTree a]
    deriving Show

rtree1 :: RTree Int
rtree1 = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

--                           6
--                   /  \       \    \
--                 4     3       6   11
--               /  \     \
--             7     9     12
--           /  \
--         1     3

rtree2 :: RTree Int
rtree2 = R 6 [R 4 [R 7 [],
                   R 3 []],
              R 3 [R 6 [],
                   R 3 []]]

-- a)
soma :: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l)

--  soma rtree2 = 6 + sum [soma [R 4 [R 7 [] , R 3 [] ]] , soma [R 3 [R 6 [] , R 3 [] ]]]
--                6 + sum [ [4 + sum (map (soma) [R 7 [] , R 3 [] ])] , [3 + sum (map (soma) [R 6 [] , R 3 []])] ]
--                6 + sum [ [4 + sum [7 , 3]] , [3 + sum [6 , 3]]]
--                6 + sum [ [4 + (7 + 3)] , [3 + (6 + 3)]]
--                6 + (14 + 12)
--                6 + 26
--                32

-- b)
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)

-- c)
prune :: Int -> RTree a -> RTree a
prune 1 (R a l) = R a []
prune n (R a l) = R a (map (prune (n-1)) l)

-- d)
mirror :: RTree a -> RTree a
mirror (R a l) = R a (map mirror (reverse l))

-- e)
postorder :: RTree a -> [a]
postorder (R a l) = concatMap postorder l ++ [a]

-- Exercício 3
data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

data LTree a = Tip a | Fork (LTree a) (LTree a)
    deriving Show

ltree1 :: LTree Int
ltree1 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))

-- a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork l r) = ltSum l + ltSum r

-- ltSum Fork (Fork (Tip 5) (Fork (Tip 6) (Tip 4))) (Fork (Fork (Tip 3) (Tip 7)) (Tip 5)) =
--        = ltSum (Fork (Tip 5) (Fork (Tip 6) (Tip 4))) + ltSum (Fork (Fork (Tip 3) (Tip 7)) (Tip 5))
--        = ltSum (Tip 5) + ltSum (Fork (Tip 6) (Tip 4)) + ltSum (Fork (Tip 3) (Tip 7) + ltSum (Tip 5)
--        = 5 + ltSum (Tip 6) + ltSum (Tip 4) + ltSum (Tip 3) + ltSum (Tip 7) + 5
--        = 5 + 6 + 4+ 3 + 7 + 5
--        = 30


-- b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork l r) = listaLT l ++ listaLT r

-- c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r)

-- Exercício 4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
    deriving Show

-- BTree a = Empty | Node a (BTree a) (BTree a)
-- LTree a = Tip a | Fork (LTree a) (LTree a)

ftree1 :: FTree Int Int
ftree1 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))

-- a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf a) = (Empty, Tip a)
splitFTree (No a l r) = (Node a l1 r1, Fork l2 r2)
    where (l1,l2) = splitFTree l
          (r1,r2) = splitFTree r

-- b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Node a Empty Empty) (Tip b) = Just (Leaf b)
joinTrees (Node a l r) (Fork e d) = Just (No a x y)
    where Just x = joinTrees l e
          Just y = joinTrees r d
joinTrees _ _ = Nothing
