-- Exercício 1
-- a)
perimetro :: Float -> Float
perimetro r = 2 * pi * r

-- b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

-- c)
primUlt :: [Int] -> (Int,Int)
primUlt l = (head l,last l)

-- d)
multiplo :: Int -> Int -> Bool
multiplo m n = m `mod` n == 0

truncaImpar :: [Int] -> [Int]
truncaImpar l = if (length l) `mod` 2 == 0
                    then l
                else tail l

-- e)
max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = if x > c then x else c
    where x = max2 a b


-- Exercício 2
-- a)
nRaizes :: Int -> Int -> Int -> Int
nRaizes a b c | delta > 0 = 2
              | delta == 0 = 1
              | delta < 0 = 0
    where delta = b^2-4*a*c

--b)


-- Exercício 3
type Hora = (Int,Int)

-- a) Verifica se a Hora é valida
testaHora :: Hora -> Bool
testaHora (h,m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

-- b) Compara se h1 é maior que h2
comparaHora :: Hora -> Hora -> Bool
comparaHora (h1,m1) (h2,m2) | h1 > h2 = True
                            | h1 == h2 && m1 > m2 = True
                            | otherwise = False

-- c) Converte Horas para Minutos
hora2Min :: Hora -> Int
hora2Min (h,m) = h*60 + m

-- d) Converte Minutos para Horas
min2Hora :: Int -> Hora
min2Hora n = (n `div` 60 , n `mod` 60)

-- e) Diferença de Horas
difHoras :: Hora -> Hora -> Int
difHoras (h1,m1) (h2,m2) = hora1 - hora2
    where hora1 = hora2Min (h1,m1)
          hora2 = hora2Min (h2,m2)

-- f) Adicionar Minutos a uma Hora
addMin :: Int -> Hora -> Hora
addMin m h = min2Hora $ m + hora2Min h


-- Exercício 4
data Hora_ = H Int Int
    deriving (Show,Eq)

-- a) Verifica se a Hora é valida
testaHora_ :: Hora_ -> Bool
testaHora_ (H h m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

-- b) Compara se h1 é maior que h2
comparaHora_ :: Hora_ -> Hora_ -> Bool
comparaHora_ (H h1 m1) (H h2 m2) | h1 > h2 = True
                                 | h1 == h2 && m1 > m2 = True
                                 | otherwise = False

-- c) Converte Horas para Minutos
hora2Min_ :: Hora_ -> Int
hora2Min_ (H h m) = h*60 + m

-- d) Converte Minutos para Horas
min2Hora_ :: Int -> Hora_
min2Hora_ n = H (n `div` 60) (n `mod` 60)

-- e) Diferença de Horas
difHoras_ :: Hora_ -> Hora_ -> Int
difHoras_ (H h1 m1) (H h2 m2) = hora1 - hora2
    where hora1 = hora2Min_ (H h1 m1)
          hora2 = hora2Min_ (H h2 m2)

-- f) Adicionar Minutos a uma Hora
addMin_ :: Int -> Hora_ -> Hora_
addMin_ m h = min2Hora_ $ m + hora2Min_ h


-- Exercício 5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a)
next :: Semaforo -> Semaforo
next x = case x of
         Verde -> Amarelo
         Amarelo -> Vermelho
         Vermelho -> Verde

-- b)
stop :: Semaforo -> Bool
stop x = x == Vermelho

-- c)
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = if s1 == s2
                 then False
             else True

-- Exercício 6
data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)

