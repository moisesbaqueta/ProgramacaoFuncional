-- Exercício 1
data Hora = H Int Int
    deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- FUNÇÕES IMPORTADAS DA FICHA 1

-- Verifica se a Hora é valida
testaHora :: Hora -> Bool
testaHora (H h m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

-- Compara se h1 é maior que h2
comparaHora :: Hora -> Hora -> Bool
comparaHora (H h1 m1) (H h2 m2) | h1 > h2 = True
                                 | h1 == h2 && m1 > m2 = True
                                 | otherwise = False

-- Converte Horas para Minutos
hora2Min :: Hora -> Int
hora2Min (H h m) = h*60 + m

-- Converte Minutos para Horas
min2Hora :: Int -> Hora
min2Hora n = H (n `div` 60) (n `mod` 60)

-- Diferença de Horas
difHoras :: Hora -> Hora -> Int
difHoras (H h1 m1) (H h2 m2) = hora1 - hora2
    where hora1 = hora2Min (H h1 m1)
          hora2 = hora2Min (H h2 m2)

-- Adicionar Minutos a uma Hora
addMin :: Int -> Hora -> Hora
addMin m h = min2Hora $ m + hora2Min h


-- a) Testa se uma etapa está bem contruída
testaEtapa :: Etapa -> Bool
testaEtapa (h1,h2) = testaHora h1 && testaHora h2 && comparaHora h2 h1

-- b) Testa se uma viagem está bem construída
testaViagem :: Viagem -> Bool
testaViagem [] = True
testaViagem [h] = True
testaViagem ((h1,h2):(h3,h4):t) = if (testaEtapa (h1,h2) && comparaHora h3 h2)
                                      then testaViagem ((h3,h4):t)
                                  else False

-- c) Calcula a Hora de Partida e a Hora de Chegada
partida_chegada :: Viagem -> (Hora,Hora)
partida_chegada [(h1,h2)] = (h1,h2)
partida_chegada (h:t) = (fst h,snd (last t))

--d) Calcula o tempo total da Viagem (válida) em Minutos
tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem ((h1,h2):t) = difHoras h2 h1 + tempoViagem t

-- e) Calcula o tempo de Espera em Minutos
tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera [(h1,h2)] = 0
tempoEspera ((h1,h2):(h3,h4):t) = difHoras h3 h2 + tempoEspera ((h3,h4):t)

-- f) Calcula o tempo Total da Viagem (tempo de espera + tempo de viagem)
tempoTotal :: Viagem -> Int
tempoTotal v = tempoViagem v + tempoEspera v


-- Exercício 3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
    deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

agenda1 = [("João", [Casa 123456789, Tlm 987654321, Email "jon@gmail.com", Email "jon23@gmail.com"]),("Nuno", [Tlm 965555555]),("Tónio", [Trab 253551188])]

-- a) Adiciona um Email para um dado Mome da Agenda
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n m [] = [(n,[Email m])]
acrescEmail n m ((nome, c):t) = if n == nome
                                    then ((nome,c ++ [Email m]):t)
                                else (nome,c) : acrescEmail n m t

-- b) Dado um Nome e uma Agenda devolve os Emails desse Nome
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome ((n,l):xs) = if nome == n
                                then Just (emails l)
                            else verEmails nome xs

emails :: [Contacto] -> [String]
emails [] = []
emails (h:t) = case h of
    Email a -> a : emails t
    otherwise -> emails t

-- c) Indica a lista de numeros
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of
                   Casa a -> a : consTelefs t
                   Trab a -> a : consTelefs t
                   Tlm a -> a : consTelefs t
                   otherwise -> consTelefs t

-- d) Indica o numero de telefone de casa de uma dada pessoa
casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Just 0
casa n ((nome,c):t) = if n == nome
                          then Just (recolheCasa c)
                      else casa n t

recolheCasa :: [Contacto] -> Integer
recolheCasa (x:xs) = case x of
                     Casa a -> a
                     otherwise -> recolheCasa xs


-- Exercício 4
type Dia = Int
type Mes = Int
type Ano = Int
type Nome_ = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

lista1 = [("Ana", (D 12 08 2002)), ("Joao", (D 23 08 2002))]

-- a) Indica a data de nascimento de uma dada pessoa
procura :: Nome_ -> TabDN -> Maybe Data
procura n [] = error "Não existe!"
procura n ((nome,d):t) = if n == nome
                             then Just d
                         else procura n t

-- b) Calcula a Idade de uma pessoa
idade :: Data -> Nome -> TabDN -> Maybe Int
idade dA n [] = Just 0
idade dA n ((nome,dN):t) = if n == nome
                               then Just (calculaIdade dA dN)
                           else idade dA n t

calculaIdade :: Data -> Data -> Ano
calculaIdade (D d1 m1 a1) (D d2 m2 a2) | m1 > m2 || m1 == m2 && d1 >= d2 = a1 - a2
                                       | otherwise = a1-a2-1

-- c) Testa se uma data é anterior a outra
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) | a1 < a2 = True
                                   | a1 == a2 && m1 < m2 = True
                                   | otherwise = False

-- d) Ordena por ordem crescente das datas de nascimento
ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = ordenaAux h (ordena t)
    where ordenaAux h [] = [h]
          ordenaAux (nome1,d1) ((nome2,d2):t) = if anterior d2 d1
                                                    then (nome2,d2) : ordenaAux (nome1,d1) t
                                                else (nome1,d1):(nome2,d2):t

-- ordena [("Ana", (D 12 08 2002)) , ("Joao", (D 23 08 2002)) , ("Tonny", (D 10 10 1990))]
-- = ordenaAux ("Ana", (D 12 08 2002)) : ordena (("Joao", (D 23 08 2002)), ("Tonny", (D 10 10 1990)))
-- = ordenaAux ("Ana", (D 12 08 2002)) : ordenaAux ("Joao", (D 23 08 2002)) : ordena ("Tonny", (D 10 10 1990))
-- = ordenaAux ("Ana", (D 12 08 2002)) : ordenaAux ("Joao", (D 23 08 2002)) : ordenaAux ("Tonny", (D 10 10 1990)) []
-- = ordenaAux ("Ana", (D 12 08 2002)) : ordenaAux ("Joao", (D 23 08 2002)) : [("Tonny", (D 10 10 1990))]
-- = ordenaAux ("Ana", (D 12 08 2002)) : [("Tonny", (D 10 10 1990)) , ("Joao", (D 23 08 2002))]
-- = [("Tonny", (D 10 10 1990)) , ("Ana", (D 12 08 2002)) , ("Joao", (D 23 08 2002))]


-- e) Numa dada data, apresenta uma lista por ordem crescente de Idade
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) l = (n,idade) : porIdade (D d m a) ts
    where ((n,D dx mx ax):ts) = ordena l
          idade = if m > mx || mx == m && d > dx
                      then (a-ax)
                  else ((a-ax)-1)

-- Exercício 5
data Movimento = Credito Float | Debito Float
    deriving Show
data Data_ = D_ Int Int Int
    deriving Show
data Extracto = Ext Float [(Data_, String, Movimento)]
    deriving Show

-- a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext a []) _ = []
extValor (Ext a ((_,_,mov):t)) x = case mov of
                                   Credito c -> if c >= x then mov : extValor (Ext a t) x else extValor (Ext a t) x
                                   Debito c -> if c >= x then mov : extValor (Ext a t) x else extValor (Ext a t) x

-- b)
filtro :: Extracto -> [String] -> [(Data_,Movimento)]
filtro (Ext a []) m = []
filtro _ [] = []
filtro (Ext a ((dat,m1,mov):t)) m2 = if m1 `elem` m2
                                          then (dat,mov) : filtro (Ext a t) m2
                                      else filtro (Ext a t) m2

-- c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext a []) = (0,0)
creDeb (Ext a ((_,_,mov):t)) = case mov of
                               Credito x -> (x+a,b)
                               Debito x -> (a,x+b)
    where (a,b) = creDeb (Ext a t)

-- d)
saldo :: Extracto -> Float
saldo (Ext a b) = (a-x+y)
    where (x,y) = creDeb (Ext a b)
