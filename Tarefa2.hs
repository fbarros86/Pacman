{- |
Module : Tarefa2
Description : Tarefa2 - Trabalho de LI1 2020/2021

= Introdução

Nesta tarefa, definimos o resultado da movimentação de um jogador num estado de jogo

>play :: Play -> State -> State

= Objetivo

Para percebermos o efeito da jogada pretendida no jogo começamos por
perceber se o jogador vai apenas mudar de direção ou se vai avançar.
Se ele for avançar, procura-se ver se este irá ao encontro de outro jogador,
aplicando as respetivas consequências se tal acontecer.

De seguida, move-se o jogador alterando o estado do jogo de acordo com a peça que
se encontra no posição seguinte

Finalmente, verifica-se também se as comidas todas do labirinto já foram comidas,
se for o caso, o nível muda. Consequentemente, o tempoMega do pacman diminui, a velocidade
dos fantasmas no modo Alive aumenta e o labrinto é atualizado para um labirinto
pré-definido.

= Discussão e conclusão

Considerámos que a velocidade dos fantasmas à medida que os níveis passam aumenta
0.5, no entanto, para um máximo de 2, porque se não o pacman morriria muito rapidamente.
Além disso, o tempoMega também diminui, mas para um mínimo de 5 segundos, para não
tornar o jogo demasiado difícil.

Após testar os diferentes cenários possíveis, concluímos que o nossa função comporta-se como esperado.  

-}

module Tarefa2 where

import Types
import FileUtils
import Tarefa1
import System.Random
import System.IO.Unsafe


{- | Move um jogador (identificado pelo seu identificador único (ID))
numa determinada direção e atualizando o efeito dessa jogada no labirinto atual

-}
play :: Play -> State -> State
play p (State m ps l) = play_ p m player outrosP l
                        where (player, outrosP) = separaPlayers p ps



{- | Verifica se um jogador se movimenta numa direção que não a sua direção atual, se for o caso
 a sua posição mantêm-se apenas se altera a orientação

 Se não for o caso, descobre se existe algum/alguns jogadores na posição para onde se move.
 Se não existirem este apenas avança de posição, alterando possivelmente alguns dos seus
 atributos consoante a peça que se encontra nesta mesma posição
 Se existirem este altera os atributos dos jogadores que se encontram e, de seguida, faz algo semelhante
 ao caso anterior, ou seja, avança de posição e sofre algumas alterações consoante a peça que se
 encontra na posição para onde se move 

-}
play_ :: Play -> Maze -> Player -> [Player] -> Int -> State
play_ (Move n o) m player outrosP  l
      | getPlayerOrientation player /= o = (State m (inserePlayer (mudaOrientacao player o) outrosP) l)
      | (pCoords == []) = proximoMovimento (Move n o) m player outrosP l peca
      | isDying player' = State m (inserePlayer player' outrosP') l
      | otherwise = proximoMovimento (Move n o) m player' (outrosP' ++ plrs) l peca
      where (xf, yf) = proximaPosicao m (getPlayerCoords player) o 
            peca = (m !! xf) !! yf
            (pCoords,plrs) = playersInCoords (xf,yf) outrosP
            (player',outrosP') = mudaPlayers m player pCoords l


-- | Separa o jogador que se vai mover dos restantes
--
separaPlayers:: Play -> [Player] -> (Player, [Player])
separaPlayers _ [] = error "Player not found"

separaPlayers m@(Move n o) (p:ps) 
      | (getPlayerID p == n) = (p, ps)
      | otherwise = (pCerto, p: outrosP)
 where (pCerto, outrosP) = separaPlayers m ps


-- | Vê se se trata de um pacman no modo Dying
--
isDying :: Player -> Bool
isDying (Pacman (PacState ps q c Dying )) = True
isDying _ = False


-- | Divide os jogadores que se encontram numas certas coordenadas c dos restantes
--
-- Podia também ser definida sa seguinte forma:
--
-- > playersInCoords c = (filter ((== c).(getPlayerCoords)) ,filter ((/= c).(getPlayerCoords)) )
--
playersInCoords :: Coords -> [Player] -> ([Player],[Player])
playersInCoords c [] = ([],[])
playersInCoords c (p:ps) | getPlayerCoords p == c = (p:a,b)
                         | otherwise = (a,p:b)
                         where (a,b) = playersInCoords c ps


{- | Altera o estado dos jogadores quando eles se cruzam, consoante o estado de cada um

- Quando um Pacman encontra um ghost no modo Alive, pode acontecer uma de duas coisas.
Se este ainda tiver vidas, perde uma vida . Se o Pacman já não tiver vidas, este altera o seu estado para Dying

- Quando um Pacman encontra um ghost no modo Dead, este obtem +10 pontos, enquanto que o ghost desaparece, reeaparecendo
dentro da casa dos fantasmas em modo Alive


-}
mudaPlayers :: Maze -> Player -> [Player] -> Int -> (Player, [Player])
mudaPlayers m p [] l = (p,[])
mudaPlayers m (Pacman pacs) (g1@(Ghost (GhoState ps Alive)):gs) level
          = if (l >= 1) then (pacmanMenosVida, g1:ghosts1)
            else (Pacman (PacState (x,y,z,t,h,l) q c Dying ) , g1:gs)
            where (PacState (x,y,z,t,h,l) q c d )= pacs
                  (pacmanMenosVida,ghosts1) = mudaPlayers m (Pacman (PacState (x,y,z,t,h,l-1) q c d )) gs l
mudaPlayers m (Pacman pacs) (g1@(Ghost (GhoState ps Dead)):gs) level
          = (pacmanPontos, g1': ghosts)
          where (PacState (x,y,z,t,h,l) q c d ) = pacs
                (Ghost (GhoState (x',y',z',t',h',l') q' )) = g1
                g1' = (Ghost (GhoState (x',( div (linhas m - 1) 2, div (colunas m -1) 2),vAlive level,U,h',l') Alive ))
                (pacmanPontos, ghosts) = mudaPlayers m (somaPontos 10 (Pacman pacs)) gs l
mudaPlayers m g@(Ghost (GhoState ps Alive)) (Pacman pacs:plrs) level
          = if (l>=1) then (g' , (Pacman (PacState (x,y,z,t,h,l-1) q c d )):gs)
            else (g', (Pacman (PacState (x,y,z,t,h,l) q c Dying ):gs))
            where (PacState (x,y,z,t,h,l) q c d ) = pacs
                  (g',gs) = mudaPlayers m g plrs l
mudaPlayers m g@(Ghost (GhoState ps Dead)) (p@(Pacman p1):plrs) level
          = (g', pac:plrs)
          where (Ghost (GhoState (x,y,z,t,h,l) q )) = g
                pac = somaPontos 10 p
                g' = (Ghost (GhoState (x,( div (linhas m - 1) 2, div (colunas m - 1) 2),vAlive level,U,h,l) Alive ))
mudaPlayers m g (gh:t) l = (g',gh:pls)
                         where (g',pls) = mudaPlayers m g t l
                                                      
-- | Retorna a velocidade com que o fantasma fica quando passa ao
--modo Alive, de acordo com o nível do jogo 
--
vAlive :: Int -> Double
vAlive l | l==1 = 1
         | l==2 = 1.5
         | otherwise = 2


{- | Retorna um State de acordo com o player dado e com a peça dada (peça que se encontra na posição para onde se move)

__ Se o jogador for um pacman: __

- Quando transita para uma posição vazia, nada acontece;

- Quando transita para uma posição com comida pequena, a sua pontuação é atualizada com +1 ponto;

- Quando transita para uma posição com comida grande,
a sua pontuação é atualizada com +5 pontos, o seu estado muda para Mega e o timeMega muda para tMega.
Além disso, os fantasmas existentes no labirinto mudam o seu estado para Dead
e diminuem a sua velocidade para metade.

- Quando a peça dada for uma parede, este não altera a sua posição

-}
proximoMovimento :: Play -> Maze -> Player -> [Player] -> Int -> Piece -> State
proximoMovimento p m player plys l Wall 
          = State m (inserePlayer player plys) l
proximoMovimento p m (Pacman ps) ghosts l Empty
          = avancaPosicao p m (Pacman ps) ghosts l
proximoMovimento p m (Pacman ps) ghosts l (Food Little)
          = avancaPosicao p m (somaPontos 1 (Pacman ps)) ghosts l

proximoMovimento p m (Pacman ps) ghosts l (Food Big)
          = avancaPosicao p m (somaPontos 5 (Pacman (PacState (x,y,z,t,h,l) (tempoML l) c Mega ))) (ghostsDead ghosts) l
          where (PacState (x,y,z,t,h,l) q c d )= ps
proximoMovimento (Move n o) m g@(Ghost gs) outrosP l _ 
          = State m (inserePlayer (setPlayerCoords g cF) outrosP) l
          where c = getPlayerCoords g
                cF = proximaPosicao m c o

-- | Calcula o tempo que o Pacman fica no modo Mega após comer uma comida grande
-- (que vai diminuindo com as passagens de nível)
-- 
tempoML :: Int -> Double
tempoML l = if (l<6) then fromIntegral (11 - l)
            else fromIntegral 5

-- | Retorna um estado após avançar o jogador um casa (deixando a posição onde estava com a peça Empty)
--
avancaPosicao:: Play -> Maze -> Player -> [Player] -> Int -> State
avancaPosicao (Move n o) m player outrosP l = mudarDeNivel (State mAze (inserePlayer (changeMouth(setPlayerCoords player cF)) outrosP) l)
                                              where mAze = replaceElemInMaze c Empty (replaceElemInMaze cF Empty m)
                                                    c = getPlayerCoords player
                                                    cF = proximaPosicao m c o
-- | Altera o estado da boca do Pacman
--
changeMouth :: Player -> Player
changeMouth (Pacman (PacState ps q Open d)) = (Pacman (PacState ps q Closed d))
changeMouth (Pacman (PacState ps q Closed d)) = (Pacman (PacState ps q Open d))

-- | Soma x pontos ao jogador
--
somaPontos :: Int -> Player -> Player
somaPontos pontos (Pacman (PacState (x,y,z,t,h,l) q c d ))
          = (Pacman (PacState (x,y,z,t,h+pontos,l) q c d ))
somaPontos pontos (Ghost (GhoState (x,y,z,t,h,l) q ))
          = (Ghost (GhoState (x,y,z,t,h+pontos,l) q ))

-- | Altera o estado de todos os fantasmas para Dead e diminui a sua velocidade para metade
--
-- __Obs:__ considera que todos os jogadores dados são fantasmas 
--
ghostsDead :: [Player] -> [Player]
ghostsDead l = map mudaGhost l
    where mudaGhost (Ghost (GhoState (x,y,z,t,h,l) q ))
            = Ghost (GhoState (x,y,0.5,(turnRight (turnRight t)),h,l) Dead)


-- | Insere um jogador numa lista e jogadores de forma a que na lsita resultante os IDs dos jogadores
-- apareçam por ordem crescente
--
inserePlayer :: Player -> [Player] -> [Player]
inserePlayer p [] = [p]
inserePlayer p1 (p2:ps) = if (pid1 < pid2) then p1:p2:ps
                          else p2: inserePlayer p1 ps
                          where pid1 = getPlayerID p1
                                pid2 = getPlayerID p2

-- | Atualiza o estado do jogo para a mudança de nível, aumentando a velocidade dos fantasmas
-- e atualizando o labirinto 
--
mudarDeNivel :: State -> State
mudarDeNivel s@(State m ps l) = if (comComida m) then s
                                else State (mudaMaze m) (mudaVelocidadeP ps) (l+1)

-- | Verifica se ainda existe alguma comida no labirinto
--
comComida :: Maze -> Bool
comComida = any (any food) 
            where food (Food Big) = True
                  food (Food Little) = True
                  food _ = False

-- | Muda a velocidade dos fantasmas (quando se muda de nível) e a posição dos diferentes jogadores.
-- O pacman vai para uma posição central, por baixo da casa dos fantasmas e os fantasmas
-- aparecem numa das posições possíveis dentro da casa dos fantasmas, aleatoriamente.
--
mudaVelocidadeP :: [Player] -> [Player]
mudaVelocidadeP = map mudaVP
     where mudaVP g@(Ghost (GhoState (x,y,z,t,h,l) Alive)) =
              if (z>=2) then setPlayerCoords g (5,8)
              else setPlayerCoords (Ghost (GhoState (x,y,z+0.5,t,h,l) Alive )) (4,(unsafePerformIO (randomRIO (9,15))))
           mudaVP p@(Pacman ps) = setPlayerCoords p (6,12)
           mudaVP g = setPlayerCoords g (4,(unsafePerformIO (randomRIO (9,15))))


-- | Retorna o labirinto do novo nível
--
mudaMaze :: Maze -> Maze
mudaMaze m = replaceElemInMaze (8,23) (Food Big)(replaceElemInMaze (8,17) (Food Big) (replaceElemInMaze (6,22) (Food Little) (replaceElemInMaze (5,1) (Food Little) (generateMaze 25 10 120111234443434444))))
--mudaMaze m = replaceElemInMaze (5,1) (Food Little) (replaceElemInMaze (9,6) (Food Big) (generateMaze 18 12 1155997))
                                                               