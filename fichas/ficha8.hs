import Data.List
import Data.Char

-- Exercício 1
data Frac = F Integer Integer

-- a)
normaliza :: Frac -> Frac
normaliza (F x y) =  F (x `div` (mdc x y)) (y `div` (mdc x y))

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (mod x y)

-- b)
instance Eq Frac where
    (F x y) == (F x1 y1) = (x * y1) == (x1 * y)
-- ou
--    normaliza (F x y) == normaliza (F x1 y1)

-- EXEMPLO
-- 2 / 4 == 1 / 2      ou    2    1      2 * 2 == 1 * 4 = True
-- 1 / 2 == 1 / 2            4    2
-- TRUE


-- ORD é definido por LT <
--                    EQ =
--                    GT >

-- c)
instance Ord Frac where
    compare (F x1 y1) (F x2 y2) | c1 < c2 = LT
                                | c1 == c2 = EQ
                                | c1 > c2 = GT
        where c1 = fromIntegral x1 / fromIntegral y1
              c2 = fromIntegral x2 / fromIntegral y2

-- d)
instance Show Frac where
    show (F x y) = show x ++ "/" ++ show y

-- Num é definido:
    -- (+), (*), (-) :: a -> a -> a
    -- negate, abs, signum :: a -> a
    -- fromInteger :: Integer -> a

-- e)
instance Num Frac where
    (F x1 y1) + (F x2 y2) | y1 == y2 = normaliza $ F (x1 + x2) y1
                          | otherwise = normaliza $ F (x1*y2 + x2*y1) (y1*y2)

    (F x1 y1) * (F x2 y2) = normaliza $ F (x1*x2) (y1*y2)

    (F x1 y1) - (F x2 y2) | y1 == y2 = normaliza $ F (x1 - x2) y1
                          | otherwise = normaliza $ F (x1*y2 - x2*y1) (y1*y2)

    negate (F x y) = F (-x) y

    abs (F x y) = F (abs x) (abs y)

    signum (F x y) | x == 0 = 0
                   | x * y > 0 = 1
                   | otherwise = -1

    fromInteger x = F x 1

-- f)
maior2x :: Frac -> [Frac] -> [Frac]
maior2x f l = filter (> 2 * f) l


-- Exercício 2
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- a)
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

-- b)
valorDe :: (Num a) => Exp a -> a
valorDe (Const a) = a
valorDe (Simetrico a) = - (valorDe a)
valorDe (Mais a b) = valorDe a + valorDe b
valorDe (Menos a b) = valorDe a - valorDe b
valorDe (Mult a b) = valorDe a * valorDe b

instance (Num a,Eq a) => Eq (Exp a) where
    x == y = valorDe x == valorDe y

-- c)
instance (Num a, Eq a) => Num (Exp a) where
    x + y = Const (valorDe x + valorDe y)

    x - y = Const (valorDe x - valorDe y)

    x * y = Const (valorDe x * valorDe y)

    negate (Const a) = Const (- a)
    negate (Simetrico a) = a
    negate (Mais a b) = Mais (- a) (- b)
    negate (Menos a b) = Menos b a
    negate (Mult a b) = Mult (-a) b

    fromInteger x = Const (fromInteger x)

    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs (Mais a b) = abs (a + b)
    abs (Menos a b) = abs (a - b)
    abs (Mult a b) = abs (a * b)

    signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
    signum (Simetrico a) = - signum a
    signum (Mais a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
    signum (Menos a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
    signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))


-- Exercício 3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

-- a)
instance Ord Data where
     compare (D d1 m1 a1) (D d2 m2 a2) | a1 > a2 || a1 == a2 && m1 > m2 || a1 == a2 && m1 == m2 && d1 > d2 = GT
                                       | a1 == a2 && m1 == m2 && d1 == d2 = EQ
                                       | otherwise = LT

-- b)
instance Show Data where
    show (D d m a) = "Dia " ++ show d ++ " do mês " ++ show m ++ " do ano " ++ show a

-- c)
ordena :: Extracto -> Extracto
ordena (Ext n l) = (Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l))

-- d)
instance Show Extracto where
    show (Ext n l) = "Saldo anterior: " ++ show n ++
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------\n" ++ concatMap (\(dat,str,mov) -> show dat ++ replicate (11 - (length (show dat))) ' ' ++ map (toUpper) str ++ "    \n") l ++
                     "---------------------------------------" ++
                     "\nSaldo actual: " ++ show (saldo (Ext n l))

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc + n)
                                                        Debito n -> (acc - n)) x lm
