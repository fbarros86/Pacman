
{- | 
Module : Tarefa5
Description : Tarefa5 - Trabalho de LI1 2020/2021

= Introdução

A tarefa 5 centra-se a empreender o comportamento dos fantasmas.

= Objetivo

O objetivo desta tarefa é definir a função ghostPlay cujo input é o Estado dos Fantasmas.
O resultado deverá ser uma lista de jogadas que o fantasma realizará de modo a atingir
o seu objetivo, dependendo do seu Estado (Dead ou Alive).

> ghostPlay :: State -> [Play]

Teremos então duas hipóteses de ação dos fantasmas:

- chaseMode;

- scatterMode.

__chaseMode__ : quando os fantasmas estão Alive e o Pacman está Normal, os fantasmas irão
perseguir o Pacman.

__scatterMode__ : quando os fantasmas estão Dead e o Pacman em modo Mega, os fantasmas terão
que fugir do Pacman.

= Discussão e Conclusão:

Após várias tentivas de funções, decidimos que a função que usa uma lista de coordenadas e orientação para detereminar o melhor caminho até 
às coordenadas do Pacman naquele momento seria a mais eficiente pois evita que o fantasma fique preso em becos. De seguida testamos e 
confirmamos a eficiência desta função.

-}

module Tarefa5 where 

import Types
import System.Random
import System.IO.Unsafe


ghostPlay :: State -> [Play]
ghostPlay s@(State m ps l) = map (veFantasmas s) (semPacman ps)


-- | Lista de Jogadores sem o Pacman.
--
semPacman :: [Player] -> [Player]
semPacman [(Pacman p)] = []
semPacman ((Pacman p):t) = t
semPacman (p:ps) = p: semPacman ps

{- | 
Prevê o estado que o fantasma terá na jogada seguinte e de acordo com o estado aplica uma de duas funções.

-}
veFantasmas :: State -> Player -> Play
veFantasmas s@(State m ps l) g = case g of
                                  (Ghost (GhoState gs Dead)) -> scatterMode s (getPlayerID g)
                                  (Ghost (GhoState gs Alive)) -> chaseMode s (getPlayerID g)
                  

{-| Dependendo do estado do fantasma naquele momento, temos duas hipóteses de comportamento dos fantasmas:

- Quando um fantasma está em modo Alive, vamos aplicar o chaseMode.
O fantasma irá perseguir o Pacman, determoinando a posição deste último sempre que bate numa parede ou o Pacman ou mude de Estado (Alive -> Dead).

> chaseMode :: State -> Int -> Play

-}
chaseMode :: State -> Int -> Play
chaseMode (State m ps l) x = Move x (bestWay m cP [(cG, Null)] oG )
                           where cP = encontraPacman ps
                                 g = encontraPlayer ps x
                                 cG = getPlayerCoords g 
                                 oG = getPlayerOrientation g

{- | 

- Quando um fantasma está em modo Dead, toma o comportamento scatterMode
Sempre que o fantasma se depara com uma parede, irá mover-se sempre para a sua direita até voltar a Alive.

> scatterMode :: State -> Int -> Play

-}
scatterMode :: State -> Int -> Play
scatterMode (State m ps l) x = if (((m !! xf) !! yf) /= Wall)
                               then (Move (getPlayerID g) pO)
                               else Move x (turnRight pO)
                               where g = encontraPlayer ps x
                                     pO = getPlayerOrientation g
                                     (xf,yf) = proximaPosicao m (getPlayerCoords g) (getPlayerOrientation g)

{- | Procura numa lista de jogadores as coordenadas do pacman

-}
encontraPacman :: [Player] -> Coords
encontraPacman ((Pacman s):xs) = getPlayerCoords (Pacman s)
encontraPacman (x:xs) = encontraPacman xs

{- | 
Escolhe o melhor caminho tendo em conta o labirinto, as coordenadas do Pacman e uma lista que contém todas possibilidades de movimento dos fantasmas.
De modo a escolher o caminho que melhor se adequa, ao percorrer a lista de possibilidades de jogadas, vamos mudar a peça para Paredes
nos locais onde o ghost se encontra, impedindo que o Pacman volte para trás ou fique preso no labirinto.

-}
bestWay :: Maze -> Coords -> [(Coords,Orientation)] -> Orientation -> Orientation
bestWay m cP [] d = d
bestWay m cP ((c,o):t) d = if (cP==c) then o
                           else if (o==Null)
                                then bestWay m' cP (t++pM) d
                                else bestWay m' cP (t ++ (mudarOrient o pM)) d
                        where m' = replaceElemInMaze c Wall m
                              pM = possibilidadesMov m c d


-- | Verifica se tem parede ou não nas posições à volta do fantasma                      
--
temParede :: Maze -> Coords -> Orientation -> [(Coords,Orientation)]
temParede m c o = if (((m !! xf) !! yf) == Wall) then []
                  else [((xf,yf),o)]
                  where (xf,yf) = proximaPosicao m c o
                  
-- | Aplica a função temParede a todas as orientações.
--
possibilidadesMov :: Maze -> Coords -> Orientation ->[(Coords,Orientation)]
possibilidadesMov m c o1 = if (o1==Null)
                           then (temParede m c L ++ temParede m c R ++ temParede m c U ++ temParede m c D)
                           else temParede m c o1 ++ temParede m c o2 ++ temParede m c o3 ++ temParede m c o4
         where o2 = turnRight o1
               o3 = turnRight o2
               o4 = turnRight o3
--possibilidadesMov m c o = temParede m c o1 ++ temParede m c o2 ++ temParede m c o3 ++ temParede m c o4
--   where o1 = unsafePerformIO randomOrientation
--         o2 = turnRight o1
--         o3 = turnRight o2
--         o4 = turnRight o3

-- Retorna uma orientação gerada aleatoriamente
--
--randomOrientation :: IO Orientation
--randomOrientation  = do o <- randomRIO (1,4) :: IO Int
--                        case o of
--                          1 -> return L
--                          2 -> return R
--                          3 -> return U
--                          4 -> return D

{- | Para o ghost encontrar o pacman, primeiramente, terá que tomar a orientação Null e depois orientação que 
é a primeira da lista

-}
mudarOrient :: Orientation -> [(Coords, Orientation)] -> [(Coords, Orientation)]
mudarOrient o [] = []
mudarOrient o ((c,og):xs) = (c,o) : mudarOrient o xs