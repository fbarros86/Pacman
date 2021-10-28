{- |
Module : Tarefa4
Description : Tarefa4 - Trabalho de LI1 2020/2021

= Introdução

Nesta tarefa fazemos o estado do jogo atualizar para a
passagem de um instante de tempo

Para tal é necessário definir a função passTime cujos
inputs são um número real positivo, que representa
o número de iterações por que o jogo já passou (step)
e o estado atual do jogo.
O resultado deverá ser um novo estado em que todos os jogadores
e o mapa foram atualizados.

>passTime :: Int  -> State -> State

= Objetivo

Nesta atualização temos em conta três principais aspetos:

- a velocidade de cada jogador

- o tempoMega do Pacman

- tipo de jogador

__Velocidade__ : consoante a velocidade, cada jogador vai efetuar um diferente número de jogadas por iteração.
Considerámos que a velocidade dos jogadores pode apenas variar entre 0.5 e 2, visto que
com a adição da funcionaldiade da subida de nível, os fantasmas podem atingir, no máximo,
uma velocidade de 2.

__Tempo Mega__ : se o Pacman estiver no modo Mega, após a passagem de um instante de tempo este vai diminuir. Além disso,
caso este chegue a 0, é necessário mudar tanto o modo do Pacman como o
dos fantasmas para Normal e Alive, respetivamente.

__Tipo de jogador__ : se o jogador for um Pacman, simplesmente movimenta-se no direção atual que tem. Se for um
ghost, segue a estratégia usada na Tarefa 5 para determinar o seu movimento.

=Discussão e conclusão

Após testar, verificámos que a função passTime avança todos os jogadores consoante o esperado.


-}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5


-- | Tempo entre jogadas definido
--
defaultDelayTime = 250 -- 250 ms


passTime :: Int  -> State -> State
passTime x (State m ps l) = passTime_ x (State m ps l) ps


{- |
Altera o estado do jogo, movendo todos os jogadores de acordo com a sua velocidade.
Para poder controlar a velocidade é necessário perceber como a função se comportou na iteração
anterior, para isso, a função recebe o step. Com esta informação é possível determinar
quantas vezes o jogador deve progredir nesta iteração, utilizando a paridade deste número.


Assim, considera-se que:

- se a velocidade for 0.5, o jogador apenas avança nas iterações com um step par;

- se a velocidade for 1, o jogador avança uma vez por iteração;

- se a velocidade for 1.5, o jogador avança uma vez nas iterações impares e duas nas pares;

- se a velocidade for 2, o jogador avança duas vez por iteração.

-}
passTime_ :: Int -> State -> [Player] -> State
passTime_ x s@(State m ps l) [] = tiraTempo s ps 
passTime_ x s (p:ps) | v == 1 = passTime_ x (joga p s) ps
                     |(v == 0.5) && (mod x 2 == 1) = passTime_ x s ps
                     | v == 0.5 = passTime_ x (joga p s) ps
                     |(v == 1.5) && (mod x 2 == 1) = passTime_ x (joga p s) ps
                     |(v == 1.5) || (v == 2) = passTime_ x (joga p (joga p s)) ps
                     | otherwise = passTime_ x s ps
                     where v = getPlayerVelocity p

{- |
Dado um jogador e um estado de jogo retorna o estado resultante
de efetuar uma jogada com esse mesmo jogador

-}
joga :: Player -> State -> State
joga p@(Pacman ps) s = play (Move pid o) s
                      where pid = getPlayerID p
                            o = getPlayerOrientation p
joga g s = play (veFantasmas s g) s  

{- | Caso o pacman esteja em modo Mega,
retira-lhe o tempo equivalente a uma iteração,
verificando se este volta ao modo Normal ao não.
Caso contrário retorna o estado dado, sem alterações

-}
tiraTempo :: State -> [Player] -> State
tiraTempo s [] = s
tiraTempo s@(State m pls l) (Ghost g : ps) = tiraTempo s ps
tiraTempo s@(State m pls l) (p@(Pacman (PacState ps tM mth Mega)):t)
        | tM > ((fromInteger defaultDelayTime)/1000) = State m (mudaTempo pls) l
        | otherwise = State  m (mudaEstado pls l) l
tiraTempo s ps = s 

-- | Retira ao timeMega do Pacman o tempo equivalente a uma iteração
--
mudaTempo :: [Player] -> [Player]
mudaTempo [] = []
mudaTempo ((Pacman (PacState ps tM mth pM)):pls)
      = (Pacman (PacState ps (tM - ((fromInteger defaultDelayTime)/1000)) mth pM)): pls
mudaTempo (p:ps) = p:(mudaTempo ps)

-- | Altera todos os jogadores sabendo que o pacman irá mudar do estado Mega para Normal.
-- Ou seja, os fantasmas ficam todos no modo Alive e o Pacman além de ficar no estado Normal,
-- altera o seu timeMega para 0
--
mudaEstado :: [Player] -> Int -> [Player]
mudaEstado [] _ = []
mudaEstado ((Ghost (GhoState (x,y,z,t,h,l) q )) : pls) level = (Ghost (GhoState (x,y,vAlive level,t,h,l) Alive )): mudaEstado pls level
mudaEstado ((Pacman (PacState ps tM mth Mega)):pls) l = (Pacman (PacState ps 0 mth Normal)) : mudaEstado pls l