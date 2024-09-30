-- Exercício 1

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

a1 = Node 5 (Node 4 Empty Empty)
            (Node 7 Empty (Node 10 Empty Empty))

--           5
--        /     \
--       4       7
--                \
--                 10

a2 = Node 10 (Node 5 (Node 2 Empty Empty)
                     (Node 7 (Node 6 Empty Empty)
                             (Node 8 Empty Empty)))
             (Node 18 (Node 12 Empty Empty)
                      (Node 21 (Node 19 Empty Empty)
                               (Node 35 Empty Empty)))

--         10
--       /     \
--    5          18
--   /  \       /  \
-- 2      7   12    21
--       / \        / \
--      6   8      19 35

-- a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = 1 + (max (altura e) (altura d))

-- b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

-- c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d

-- d)
prune :: Int -> BTree a -> BTree a
prune n (Node r e d) = if n == 0
                           then Empty
                       else Node r (prune (n - 1) e) (prune (n - 1) d)

-- e)
path :: [Bool] -> BTree a -> [a]
path [] (Node r _ _) = [r]
path [] Empty = []
path l Empty = []
path (h:t) (Node r e d) = case h of
                          False -> r : path t e
                          True -> r : path t d

-- f)
mirror :: BTree a -> BTree a
mirror (Node r e d) = Node r (mirror d) (mirror e)

-- g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

-- h)
unzipBT :: BTree (a,b,c) -> (BTree a , BTree b , BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (Node a l1 r1 , Node b l2 r2 , Node c l3 r3)
    where (l1,l2,l3) = unzipBT e
          (r1,r2,r3) = unzipBT d

-- Exercício 2
-- a)
minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r e d) = minimo e

-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty _) = Empty
semMinimo (Node r e d) = Node r (semMinimo e) d

-- c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty _) = (r , Empty)
minSmin (Node r e d) = (a , Node r b d)
    where (a,b) = minSmin e

-- d)


-- Exercício 3
type Aluno = (Numero,Nome,Regime,Classificacao)

type Numero = Int

type Nome = String

data Regime = ORD | TE | MEL
    deriving Show

data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty)
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))

-- a)
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (n1,_,_,_) e d) | n < n1 = inscNum n e
                                | n > n1 = inscNum n d
                                | otherwise = True


-- b)
inscNome :: Nome -> Turma -> Bool
inscNome nome Empty = False
inscNome nome (Node (_,a,_,_) e d) = if nome == a
                                         then True
                                     else inscNome nome e || inscNome nome d

-- c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,r,_) e d) = case r of
                               TE -> [(num,nome)] ++ trabEst e ++ trabEst d
                               otherwise -> trabEst e ++ trabEst d

-- d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,_,_,c) e d) | n == num = Just c
                          | n < num = nota n e
                          | n > num = nota n d
                          | otherwise = Nothing

-- e)
percFaltas :: Turma -> Float
percFaltas t = (fromIntegral (contaFaltas t) / fromIntegral (contaAlunos t)) * 100

contaFaltas :: Turma -> Int
contaFaltas Empty = 0
contaFaltas (Node (_,_,_,c) e d) = case c of
                               Faltou -> 1 + contaFaltas e + contaFaltas d
                               otherwise -> contaFaltas e + contaFaltas d

contaAlunos :: Turma -> Int
contaAlunos Empty = 0
contaAlunos (Node r e d) = 1 + contaAlunos e + contaAlunos d


-- f)
mediaAprov :: Turma -> Float
mediaAprov t = fromIntegral (somaDasNotas t) / fromIntegral (alunosAprovados t)

somaDasNotas :: Turma -> Int
somaDasNotas Empty = 0
somaDasNotas (Node (_,_,_,c) e d) = case c of
                                    Aprov x -> x + somaDasNotas e + somaDasNotas d
                                    otherwise -> somaDasNotas e + somaDasNotas d

alunosAprovados :: Turma -> Int
alunosAprovados Empty = 0
alunosAprovados (Node (_,_,_,c) e d) = case c of
                                       Aprov x -> 1 + alunosAprovados e + alunosAprovados d
                                       otherwise -> alunosAprovados e + alunosAprovados d

-- g)
aprovAv :: Turma -> Float
AprovAv Empty = 0
AprovAv (Node (_,_,_,c) e d) = case c of
                               Aprov x -> (1 + aprovAv e + aprovAv d) / (1 + aprovAv e + aprovAv d)
                               Rep -> (aprovAv e + aprovAv d) / (1 + aprovAv e + aprovAv d)
                               otherwise -> (aprovAv e + aprovAv d) / (aprovAv e + aprovAv d)
