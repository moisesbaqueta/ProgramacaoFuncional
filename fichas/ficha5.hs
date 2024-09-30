import Data.List

-- Exercício 1
-- a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = if f h
                 then True
             else any' f t

-- b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []

-- c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) = if f h
                         then h : takeWhile' f t
                     else []

-- d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) = if f h
                         then dropWhile' f t
                     else (h:t)

-- e)
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) | f h = (h:a,[])
              | otherwise = ([],h:t)
    where (a,b) = span' f t

-- f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x (h:t) = if f x h
                         then t
                     else h : deleteBy' f x t

-- g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = inserir h (sortOn' f t)
    where inserir x [] = [x]
          inserir x (h:t) = if f x > f h
                                then h : inserir x t
                            else x : h : t

-- Exercício 2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n) p

-- b)
conta :: Int -> Polinomio -> Int
conta n p = foldl (\acc x -> if snd x == n then acc + 1 else acc) 0 p

-- c)
grau :: Polinomio -> Int
grau p = foldl (\acc x -> if acc > snd x then acc else snd x) 0 p

-- d)
deriv :: Polinomio -> Polinomio
deriv p = filter (/= (0,0)) $ map (\(c,g) -> if g > 0 then (c * (fromIntegral g),g - 1) else (0,0)) p

-- e)
calcula :: Float -> Polinomio -> Float
calcula x p = foldl (\acc (c,g) -> acc + c * x^g ) 0 p

-- f)
simp :: Polinomio -> Polinomio
simp p = filter(\x -> fst x /= 0) p

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c,g) p = map(\(c2,g2) -> (c * c2 , g + g2)) p

-- h)
ordena :: Polinomio -> Polinomio
ordena = sortOn_ (snd)

sortOn_ :: Ord b => (a -> b) -> [a] -> [a]
sortOn_ f [] = []
sortOn_ f (h:t) = inserir h (sortOn_ f t)
    where inserir x [] = [x]
          inserir x (h:t) = if f x > f h
                                then h : inserir x t
                            else x : h : t

-- i)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]

-- j)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza $ (++) p1 p2

-- k)
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl(\acc x -> soma (mult x p2) acc) [] p1

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = normaliza p1 == normaliza p2


-- Exercício 3
type Mat a = [[a]]

matriz1 :: Mat Int
matriz1 = [[1,2,3],[4,5,6],[7,8,9]]

matriz2 :: Mat Int
matriz2 = [[1,1,1],[1,1,1],[1,1,1]]

matriz3 :: Mat Int
matriz3 = [[2,1],[1,2]]

-- a)
dimOK :: Mat a -> Bool
dimOK m = all (\x -> length x == length (head m)) m

-- b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m = (length m , length (head m))

-- c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) = if dimOK (x:xs) == dimOK (y:ys)
                           then zipWith (+) x y : addMat xs ys
                       else error "Não é possivel somar matrizes de ordem diferente"

-- d)
transpose':: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m = map head m : transpose' (map tail m)

-- e)
multMat:: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = if (snd (dimMat m1) == fst (dimMat m2)) then ([[sum (zipWith (*) l1 l2) | l2 <- transpose m2] | l1 <- m1]) else (error "Not possible")

-- f)
zipWMat :: (a->b->c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2

-- g)
triSup :: (Num a,Eq a) => Mat a -> Bool
triSup [] = True
triSup (h:t) = let l = map head t
                   rm = map tail t
               in all (==0) l && triSup rm

-- h)
rotateLeft:: Mat a -> Mat a
rotateLeft m = map last m : rotateLeft (map init m)
