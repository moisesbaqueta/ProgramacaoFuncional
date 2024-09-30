import Data.Char
import Data.List
-- 1
-- a)
-- [6,12,18]

-- b)
-- [6,12,18]

-- c)
-- [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]


-- 2
-- a)
a = [2^x | x <- [0..10]]

-- b)
b = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

-- c)
c = [[1..x] | x <- [1..5]]

-- d)
d = [replicate x 1 | x <- [1..5]]

-- e)
e = [ factorial x | x <- [1..6]]
    where factorial 0 = 1
          factorial x = x * factorial (x - 1)
e' = [ product [y | y <- [1..x]] | x <- [1..6]]

-- Exercício 3
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (a,h:b)
                 | isAlpha h = (h:a,b)
                 | otherwise = (a,b)
    where (a,b) = digitAlpha t


-- Exercício 4
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (1+a,b,c)
          | h == 0 = (a,1+b,c)
          | h > 0 = (a,b,1+c)
          | otherwise = (a,b,c)
    where (a,b,c) = nzp t


-- Exercício 5
divMod' :: Integral a => a -> a -> (a,a)
divMod' a 0 = error "Divisão por 0"
divMod' a b |a<0 && b>0 || a>0 && b<0 = (-res,resp)
            |(a-b)<=0 = (x,(y+a))
            |otherwise = ((x+1),y)
    where (x,y) = (divMod' (a-b) (b) )
          (res,resp) = (divMod' (abs a) (abs b))

---------- ou ----------

divMod_ :: Integral a => a -> a -> (a, a)
divMod_ x y = foldl (\(a,b) n -> (a+1,b-y)) (0,x) [y,2*y..x]

-- Exercício 6
fromDigits' :: [Int] -> Int
fromDigits' = foldl (\acc x -> x + 10 * acc ) 0

-- Exercício 7
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits l)

-- Exercício 8
fib :: Int -> Int
fib n = acc n (0,1)
    where
        acc 0 (a,c) = a
        acc 1 (a,c) = c
        acc n (a,c) = acc (n-1) (c,a+c)
