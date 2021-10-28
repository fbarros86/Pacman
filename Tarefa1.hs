{-|
Module : Tarefa1
Description : Tarefa1 - Trabalho de LI1 2020/2021

= Introdução

Nesta tarefa desenvolvemos uma função que gera um labirinto válido para o
jogo Pacman.

> generateMaze :: Int -> Int -> Int -> Maze

= Objetivo

O labirinto que queremos criar deve cumprir vários requisitos:

1.  O labirinto é rodeado por peças Wall (exceto túnel);

2.  A faixa mais central forma um túnel;

3.  Todos os corredores têm o mesmo comprimento;

4.  Existe uma estrutura denominada “casa dos fantasmas”, localizada no centro do labirinto, que
respeita uma determinada estrutura

Para conseguirmos cumprir todos os requisitos necessários, começamos
por obter um labirinto aleatório não válido, e,de seguida,
substituimos todas as peças necessárias para o tornar válido.

= Discussão e conclusão

Após testar a função de geração de labirintos válidos,
verificámos que cumpre os requisitos listados

-}

module Tarefa1 where

import System.Random
import Types

-- | Dado o número de corredores (horizontais), o seu comprimento,
-- e um número inteiro positivo (para usar como semente num gerador pseudo-aleatório) retorna um labirinto
-- que cumpre os requisitos referidos anteriormente
--
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s | (x<15) || (y<10) = error "dimensões inválidas"
                   | otherwise = poeCasaFant y x (poeTuneis y (poeParedes l))
                    where l = converteLabirinto $ subLista x (geraAleatorios (x*y) s)

-- | Dada uma semente retorna uma lista de n inteiros gerados aleatoreamente
--
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n  $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Dada uma semente retorna um inteiro gerado aleatoriamente
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- | Transforma uma lista numa lista de listas de tamanho n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Transforma um número inteiro numa Piece, tal que: 
--
-- 3 = Food Big
--
-- 0 <= n < 70 = Food Little
--
-- 70 <= n <= 9 = Wall
--
convertePeca :: Int -> Piece
convertePeca x | x==3 = Food Big
               | x>=0 && x<70 = Food Little
               | x>=70 && x<=99 = Wall


-- | Transforma uma lista de inteiros num corredor
--
converteCorredor :: [Int] -> Corridor
converteCorredor c = map convertePeca c


-- | Transforma uma lista de inteiros num labirinto
--
converteLabirinto :: [[Int]] -> Maze
converteLabirinto m = map converteCorredor m

-- | Substitui todas peças que rodeiam o labirinto por Wall
--
poeParedes :: Maze -> Maze
poeParedes l = tudoParedes (head l): pEuParede (tail l)

-- | Substitui todas as peças de um corredor por Wall
--
-- Podia também ser definida da seguinte forma:
--
-- > tudoParedes c = map (\p -> Wall) c
--
tudoParedes :: Corridor -> Corridor
tudoParedes c = map (const Wall) c

-- | Substitui em todos os corredores a primeira e a última peça por Wall
-- (exceto no último em que esta substituição é feita para todas as peças)
--
pEuParede :: Maze -> Maze
pEuParede [c] = [tudoParedes c]
pEuParede (c1:cs) = ((Wall: init (tail c1)) ++ [Wall]): pEuParede cs

-- | Substitui, na faixa mais central, as extremidades por Empty, criando um túnel
--
-- Caso o número de linhas do labirinto for ímpar, o túnel é formado por um corredor, apenas
--
-- Caso o número de linhas do labirinto for par, o túnel é formado por dois corredor
--
poeTuneis :: Int -> Maze -> Maze
poeTuneis linhas l | (mod linhas 2 == 1) = poeTunelImpar (div (linhas+1) 2) l
                   | otherwise = poeTunelPar (div linhas 2) l

-- | Substitui as extremidades por Empty, no corredor correspondente à linha n
--
poeTunelImpar :: Int -> Maze -> Maze
poeTunelImpar n (h:t) | (n ==1) = (Empty: init (xs) ++ [Empty]) : t
                      | otherwise = h: poeTunelImpar (n-1) t
                      where (x:xs) = h

-- | Substitui as extremidades por Empty, nos corredores correspondentes à linha n e n+1 
--
poeTunelPar :: Int -> Maze -> Maze
poeTunelPar n (h:t) | (n ==1) = (Empty: init (xs) ++ [Empty]): (poeTunelPar 0 t)
                    | (n == 0) = (Empty: init (xs) ++ [Empty]):t
                    | otherwise = h: poeTunelPar (n-1) t 
                    where (x:xs) = h 

-- | Coloca a casa dos fantasmas no labirinto e substitui as peças que a rodeiam por Empty
--
-- == Propriedades
--
-- - A casa dos fantasmas tem uma altura  fixa igual a 3
--
-- - Caso o número de colunas do labirinto seja ímpar, o comprimento da casa é 9
--
-- - Caso o número de colunas do labirinto seja par, o comprimento da casa é 8
--
-- - A casa está centralizada, execto nos casos em que não é possível (quando o número de linhas é par).
-- Nestes casos a casa é colocada a X peças de distância da parede inferior e a X-1 peças de distância
-- da parede superior
--
-- - As peças que rodeiam a casa de fantasmas são Empty
--
poeCasaFant :: Int -> Int -> Maze -> Maze
poeCasaFant linhas colunas m |(mod linhas 2 == 0 && mod colunas 2 ==0) = cFantP l c m
                             | mod linhas 2 == 0 = cFantI l c m
                             | mod colunas 2 ==0 = cFantP l c m
                             | otherwise = cFantI l c m
                             where l = (div (linhas-1) 2 ) - 1
                                   c = div (colunas-10) 2

-- | Coloca no labirinto a casa dos fantasmas (com comprimento igual a 9), apartir da linha n e da coluna c
--
cFantP :: Int -> Int -> Maze -> Maze
cFantP 1 c (c1:c2:c3:c4:c5:t) = colocarChaoP c c1: colocarEntradaP c c2: colocarParedesP c c3: colocarBaseP c c4: colocarChaoP c c5 :t 
cFantP n c (h:t) = h: cFantP (n-1) c t

-- | Substitui as 10 peças após a posição x por Empty
--
-- Podia também ser definida sa seguinte forma:
--
-- @
-- colocarChaoP 0 l = replicate 10 Empty : drop 10 l
-- colocarChaoP x (h:t) = h: colocarChaoP (x-1) t
-- @
--

colocarChaoP :: Int -> Corridor -> Corridor
colocarChaoP 0 l = Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: drop 10 l
colocarChaoP x (h:t) = h: colocarChaoP (x-1) t

-- | Coloca a entrada da casa dos fantasmas (para as colunas pares)
--
colocarEntradaP :: Int -> Corridor -> Corridor
colocarEntradaP 0 l = Empty: Wall: Wall: Wall: Empty: Empty: Wall: Wall: Wall: Empty: drop 10 l
colocarEntradaP x (h:t) = h: colocarEntradaP (x-1) t

-- | Coloca as paredes laterais da casa dos fantasmas (para as colunas pares)
--
colocarParedesP :: Int -> Corridor -> Corridor
colocarParedesP 0 l = Empty:Wall: Empty: Empty: Empty: Empty: Empty: Empty: Wall: Empty: drop 10 l
colocarParedesP x (h:t) = h: colocarParedesP (x-1) t

-- | Coloca a base da casa dos fantasmas (para as colunas pares)
--
colocarBaseP :: Int -> Corridor -> Corridor
colocarBaseP 0 l = Empty: Wall: Wall: Wall: Wall: Wall: Wall: Wall: Wall: Empty: drop 10 l
colocarBaseP x (h:t) = h: colocarBaseP (x-1) t

-- | Coloca no labirinto a casa dos fantasmas caso o número de colunas seja impar
--
cFantI :: Int -> Int -> Maze -> Maze
cFantI 1 c (c1:c2:c3:c4:c5:t) = colocarChaoI c c1: colocarEntradaI c c2: colocarParedesI c c3: colocarBaseI c c4: colocarChaoI c c5:t 
cFantI n c (h:t) = h: cFantI (n-1) c t

-- | Substitui as 11 peças após a posição x por Empty
--
colocarChaoI :: Int -> Corridor -> Corridor
colocarChaoI 0 l = Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: Empty: drop 11 l
colocarChaoI x (h:t) = h: colocarChaoI (x-1) t

-- | Coloca a entrada da casa dos fantasmas (para as colunas impares)
--
colocarEntradaI :: Int -> Corridor -> Corridor
colocarEntradaI 0 l = Empty: Wall: Wall: Wall: Empty: Empty: Empty: Wall: Wall: Wall: Empty: drop 11 l
colocarEntradaI x (h:t) = h: colocarEntradaI (x-1) t

-- | Coloca as paredes laterais da casa dos fantasmas (para as colunas impares)
--
colocarParedesI :: Int -> Corridor -> Corridor
colocarParedesI 0 l = Empty: Wall: Empty: Empty: Empty: Empty: Empty: Empty: Empty: Wall: Empty: drop 11 l
colocarParedesI x (h:t) = h: colocarParedesI (x-1) t

-- | Coloca a base da casa dos fantasmas (para as colunas impares)
--
colocarBaseI :: Int -> Corridor -> Corridor
colocarBaseI 0 l = Empty: Wall: Wall: Wall: Wall: Wall: Wall: Wall: Wall: Wall: Empty: drop 11 l
colocarBaseI x (h:t) = h: colocarBaseI (x-1) t