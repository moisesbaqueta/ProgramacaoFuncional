import Data.Char

-- Exercício 2
-- a)
dobros :: [Float] -> [Float]
dobros (h:t) = h*2 : dobros t

-- b)
numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) = if c == h then 1 + numOcorre c t
                    else numOcorre c t

-- c)
positivos :: [Int] -> Bool
positivos (h:t) = if h > 0
                      then positivos t
                  else False

-- d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0
                  then h : soPos t
              else soPos t

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h < 0
                    then h + somaNeg t
                else somaNeg t

-- f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l = if length l <= 3
                then l
            else tresUlt (tail l)

-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

-- h)
nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros n [] = False
nosPrimeiros n ((a,b):t) = if n == a
                               then True
                           else nosPrimeiros n t

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(d,e,f):t) = sumTriplos ((a+d,b+e,c+f):t)


-- Exercício 3
-- a) Seleciona apenas algarismos
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if h >= '0' && h <= '9'
                      then h : soDigitos t
                  else soDigitos t

-- b) Conta letras minusculas
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if h >= 'a' && h <= 'z'
                       then 1 + minusculas t
                   else minusculas t

-- c) Seleciona apenas algarismos
nums :: String -> [Int]
nums [] = []
nums (h:t) = if h >= '0' && h <= '9'
                 then digitToInt h : nums t
             else nums t

-- Exercício 4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a) Indica quantos Monomios de grau n existe num Polinomio
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((c,g):t) = if n == g
                        then 1 + conta n t
                    else conta n t

-- b) Indica o grau de um Polinomio
grau :: Polinomio -> Int
grau [(c,g)] = g
grau ((c1,g1):(c2,g2):t) = if g1 > g2
                             then grau ((c1,g1):t)
                         else grau ((c2,g2):t)

-- c) Seleciona um certo grau
selgrau :: Int -> Polinomio -> Polinomio
selgrau x [] = []
selgrau x ((c,g):t) = if x == g
                          then (c,g) : selgrau x t
                      else selgrau x t

-- d) Derriva um Polinomio
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t) = (c * fromIntegral g ,g-1) : deriv t

-- e) Determina o valor para um certo x
calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((c,g):t) = (x * c)^g + calcula x t

-- f) Remove coeficintes 0
simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,g):t) = if c == 0
                     then simp t
                 else (c,g) : simp t

-- g) Multiplica um Monomio por um Polinomio
mult :: Monomio -> Polinomio -> Polinomio
mult (_,_) [] = []
mult (a,b) ((c,g):t) = (a*c,b+g) : mult (a,b) t

-- h) Junta Monomios do mesmo grau
normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' [(b,e)] = [(b,e)]
normaliza' ((b, e) : (b2, e2) : ps) | e == e2 = normaliza' ((b + b2, e) : ps)
                                    | conta e ps == 0 = (b, e) : normaliza' ((b2, e2) : ps)
                                    | otherwise = normaliza' ((b, e) : ps ++ [(b2, e2)])

-- i) Soma de Polinomios
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza' (p1 ++ p2)

-- j) Produto de Polinomios
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p2 = soma (mult h p2) (produto t p2)

-- k) Ordena um Polinomio
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insereMon h ( ordena t)

insereMon :: Monomio -> Polinomio -> Polinomio
insereMon x [] = [x]
insereMon (c,g) ((c1,g1):t) = if g >= g1 then (c,g) : ((c1,g1):t)
                              else (c1,g1) : insereMon (c,g) t

-- ordena [(1,3),(2,1),(4,2)] = (insereMon (1,3) (insereMon (2,1) (insereMon (4,2) [])))
--                            = (insereMon (1,3) (insereMon (2,1) : [(4,2)]))
--                            = insereMon (1,3) (4,2) : insereMon (2,1) []
--                            = insereMon (1,3) [(4,2),(2,1)]
--                            = [(1,3),(4,2),(2,1)]

-- l) Testa se 2 Polinomios são equivalentes
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza' p1) == ordena (normaliza' p2)
