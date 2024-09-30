-- 1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a == b = [a]
                | a < b = a : enumFromTo' (a+1) b
                | otherwise = []

-- 2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c | a < b && a > c = []
                      | a > b && a < c = []
                      | otherwise = a : enumFromThenTo' b ((b-a)+b) c

-- 3
concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena l [] = l
concatena [] l = l
concatena (x:xs) l2 = x : concatena xs l2

-- 4
sel :: [a] -> Int -> a
sel (h:t) n = if n == 0
                  then h
              else sel t (n-1)

-- 5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

-- 6
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (h:t) = if n == 0
                    then []
                else h : take' (n-1) t

-- 7
drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (h:t) = if n == 0
                    then (h:t)
                else drop' (n-1) t

-- 8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' l [] = []
zip' [] l = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- 9
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (h:t) = if x == h
                    then True
                else elem' x t

-- 10
replicate' :: Int -> a -> [a]
replicate' n x = if n == 0
                     then []
                 else x : replicate' (n-1) x

-- 11
interspense' :: a -> [a] -> [a]
interspense' x [h] = [h]
interspense' x [] = [x]
interspense' x (h:t) = h : x : interspense' x t

-- 12
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = takeWhile' (==h) (h:t) : group' (dropWhile' (==h) (h:t))

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) = if f h
                         then h : takeWhile' f t
                     else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) = if f h
                         then dropWhile' f t
                     else (h:t)

-- 13
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

-- 14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

-- 15
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

-- 16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] [] = True
isPrefixOf' [] l = True
isPrefixOf' l [] = False
isPrefixOf' (x:xs) (y:ys) = if x == y
                                then isPrefixOf' xs ys
                            else False

-- 17
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] [] = True
isSuffixOf' [] l = True
isSuffixOf' l [] = False
isSuffixOf' l1 l2 = if (last l1) == (last l2)
                        then isSuffixOf' (init l1) (init l2)
                    else False

-- 18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] [] = True
isSubsequenceOf' [] l = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (x:xs) (y:ys) = if x == y
                                     then isSubsequenceOf' xs ys
                                 else isSubsequenceOf' (x:xs) ys

-- 19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x l = elemIndicesAux x l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux x [] p = []
elemIndicesAux x (h:t) p = if x == h
                               then p : elemIndicesAux x t (p+1)
                           else elemIndicesAux x t (p+1)

-- 20
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if h `elem` t
                 then nub' t
             else h : nub' t

-- 21
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (h:t) = if n == h
                    then t
                  else h : delete' n t

-- 22
barrar :: Eq a => [a] -> [a] -> [a]
barrar [] [] = []
barrar [] l = []
barrar l [] = l
barrar (x:xs) (y:ys) = barrar (apagar y (x:xs)) ys

apagar :: Eq a => a -> [a] -> [a]
apagar n [] = []
apagar n (h:t) = if n == h
                   then t
                 else h : apagar n t

-- 23
union' :: Eq a =>[a] -> [a] -> [a]
union' l [] = l
union' (x:xs) (y:ys) = if elem y (x:xs)
                           then union' (x:xs) ys
                         else union' ((x:xs) ++ [y]) ys

-- 24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] [] = []
intersect' l [] = []
intersect' [] l = []
intersect' (x:xs) l2 = if elem x l2
                         then x : intersect' xs l2
                       else intersect' xs l2

-- 25
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) = if n < h
                    then n : h : t
                  else h : insert' n t

-- 26
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ " " ++ unwords' t

-- 27
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

-- 28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = if h == aux_pMaior (h:t)
                 then 0
               else 1 + pMaior t

aux_pMaior :: Ord a => [a] -> a
aux_pMaior [x] = x
aux_pMaior (h:x:t) = if h > x
                       then aux_pMaior (h:t)
                     else aux_pMaior (x:t)

-- 29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if elem h t
                       then True
                     else temRepetidos t

-- 30
algarismos :: [Char] -> [Char]
algarismos [] = ""
algarismos (h:t) = if (h >= '0') && (h <= '9')
                     then h : algarismos t
                   else algarismos t

-- 31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (h:x:t) = x : posImpares t

-- 32
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (h:x:t) = h : posPares t

-- 33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:x:t) = if h <= x
                     then isSorted (x:t)
                   else False

-- 34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (h:t) = insert'' h (iSort t)

insert'' :: Ord a => a -> [a] -> [a]
insert'' x [] = [x]
insert'' x (h:t) = if x < h
                    then x : h : t
                  else h : insert'' x t

-- 35
menor :: String -> String -> Bool
menor [] [] = False
menor l [] = False
menor [] l = True
menor (x:xs) (y:ys) | x == y = menor xs ys
                    | x > y = False
                    | x < y = True

-- 36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,b):t) = if x == a
                        then True
                      else elemMSet x t

-- 37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):t) = if b == 0
                         then lengthMSet t
                       else 1 + lengthMSet ((a,b-1):t)

-- 38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,1):t) = a : converteMSet t
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)

-- 39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) = if x == a
                           then ((a,b+1):t)
                         else (a,b) : insereMSet x t

-- 40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t) = if x == a
                           then t
                         else (a,b) : removeMSet x t

-- 41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = aux_constroiMSet h (constroiMSet t)
    where aux_constroiMSet x [] = [(x,1)]
          aux_constroiMSet x ((a,b):t) = if x == a
                                           then (a,b + 1) : t
                                         else (a,b) : aux_constroiMSet x t

-- 42
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = ((a:x),(y))
  where (x,y) = partitionEithers' t
partitionEithers' ((Right b):t) = ((x),(b:y))
  where (x,y) = partitionEithers' t

-- 43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just h):t) = h : catMaybes t
catMaybes ((Nothing):t) = catMaybes t


data Movimento = Norte | Sul | Este | Oeste deriving (Show,Eq)

-- 44
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
                        Norte -> posicao (x,y+1) t
                        Sul -> posicao (x,y-1) t
                        Este -> posicao (x+1,y) t
                        otherwise -> posicao (x-1,y) t

-- 45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | (x1 == x2) && (y1 == y2) = []
                        | x1 > x2 = Oeste : caminho (x1-1,y1) (x2,y2)
                        | x1 < x2 = Este : caminho (x1+1,y1) (x2,y2)
                        | y1 > x2 = Sul : caminho (x1,y1-1) (x2,y2)
                        | otherwise = Norte : caminho (x1,y1+1) (x2,y2)

-- 46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
                  Norte -> vertical t
                  Sul -> vertical t
                  otherwise -> False


data Posicao = Pos Int Int deriving Show

-- 47
maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral ((Pos x1 y1):(Pos x2 y2):t) = if dist (Pos x1 y1) > dist (Pos x2 y2)
                                            then maisCentral ((Pos x2 y2):t)
                                          else maisCentral ((Pos x1 y1):t)

dist :: Posicao -> Float
dist (Pos x y) = sqrt (fromIntegral ((x^2)+(y^2)))

-- 48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos a b) ((Pos x y):t) = if (a == (x - 1)) || (a == (x + 1)) || (b == (y - 1)) || (b == (y + 1))
                                     then (Pos x y) : vizinhos (Pos a b) t
                                   else vizinhos (Pos a b) t

-- 49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):t) = if y1 == y2
                                              then mesmaOrdenada ((Pos x1 y1):t)
                                            else False

-- 50
data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (h:t) = if interseccaoOK_Aux (h:t) > 1
                        then False
                      else True

interseccaoOK_Aux :: [Semaforo] -> Int
interseccaoOK_Aux [] = 0
interseccaoOK_Aux (h:t) = case h of
                            Verde -> 1 + interseccaoOK_Aux t
                            Amarelo -> 1 + interseccaoOK_Aux t
                            Vermelho -> interseccaoOK_Aux t

