{- |
Module : Tarefa6
Description : Tarefa6 - Trabalho de LI1 2020/2021

= Introdução

Nesta tarefa implementamos um robô/bot que joga Pacman automaticamente.
Este, a partir do inteiro e do estado do jogo que recebe,
analisa o que o rodeia e procura tomar a melhor decisão. 

=Objetivos

O objetivo do nosso bot é tentar obter a maior pontuação possível. Para tal,
definimos uma estratégia para cada situação do jogo.

Consideramos que quando o Pacman se encontra no modo Normal, este apenas se vai
dirigir para a comida mais próxima se não existir nenhum fantasma por perto
(cujo caminho mais curto tem uma distância inferior a 5). Se existir, este
afasta-se dele.

Quando o Pacman está no modo Mega este começa por procurar fantasmas no modo Dead
ao seu redor (que este consiga atingir com o tempo Mega que lhe resta e cuja distância
é menor que 5). Se encontrar algum dirige-se para ele, caso contrário tem um comportamento
idêntico ao Pacman no modo Normal.

=Discussão e conclusão

Após testar a eficácia do bot percebemos que ele inicialmente toma boas decisões, mas
à medida que o jogo vai avançando este começa a ter menos opções, porque a comida do
labirinto já é limitada, e começa a ter mais dificuldades.

-}


module Tarefa6 where 

import Types
import Tarefa5
import Tarefa4
import System.Random
import System.IO.Unsafe

{- | Em cada instante, o robô tem apenas conhecimento do identificador do jogador a movimentar
e do estado atual do jogo. Com esta informação, o robô vai decidir se quer efetuar alguma
jogada ou não, consoante a estratégia delineada.

-}
bot :: Int -> State -> Maybe Play
bot x s@(State m ps l) = let p = encontraPlayer ps x in
                         jogaOuNao (getPlayerOrientation p) (jogadaBot p s)


-- | Verifica se a jogada p encontrado como sendo a ideal altera o comportamento
-- normal do Pacman (mover-se na direção atual). Devolvendo Nothing caso não altere nada
-- e caso contrário devolve Just p
--
jogaOuNao :: Orientation -> Play -> Maybe Play
jogaOuNao po p@(Move x o) = if (o==po) then Nothing
                            else Just p 

-- | Procura o jogada ideal para o jogador p dado, utilizando diferentes estratégias
-- consoante o seu estado
-- 
jogadaBot :: Player -> State -> Play
jogadaBot p s@(State m ps l) = case d of
                                Normal -> botNormal p s
                                Mega -> botMega p s
                                Dying -> (Move (getPlayerID p) Null)
                               where (Pacman (PacState a b c d)) = p

{- | Com o objetivo de retornar a melhor jogada que o Pacman pode fazer quando está no modo Normal,
primeiro verifica-se se existe algum fantasma cujo comprimento de caminho mais curto seja
menor que a distGhost (definida nos Types). Se existirem fantasmas ele procura a melhor forma de se afastar deles.
Se não existirem o Pacman apenas procura dirigir-se para a comida mais próxima.

-}
botNormal :: Player -> State -> Play
botNormal p (State m ps l) =let cP = getPlayerCoords p
                                gs = (ghostsMaisProx [(cP,0,Null)] m ps oP)
                                pid = getPlayerID p
                                oP = getPlayerOrientation p
                            in
                            case gs of
                              [] -> Move pid (comidaMaisProx [(cP,Null)] m oP)
                              _ -> Move pid (decideOrientacao gs cP m oP)

{- | Retorna a lista dos pares com as orientações e o tamanho do caminho que
levam o Pacman a chegar a um fantasma (Alive).
Para as descobrir é usado o algoritmo Breadth-First, ou seja, a partir de uma posição inicial vai se vendo
quais os sítios que se pode alcançar até chegar ao objetivo, neste caso o objetivo é a distancia ultrapassar
a distGhost

-}
ghostsMaisProx :: [(Coords,Double,Orientation)] -> Maze -> [Player] -> Orientation -> [(Double,Orientation)]
ghostsMaisProx [] m ps o = []
ghostsMaisProx ((c,n,o):t) m ps d = if (n>=distGhost) then []
                                    else if (temGhostAlive c ps) then (n,o): ghostsMaisProx t m' ps d 
                                         else ghostsMaisProx (t++ (mudaPossib (n+1) o pM)) m' ps d
                                  where pM = possibilidadesMov m c d
                                        m' = replaceElemInMaze c Wall m

-- | Dada uma posição c e uma lista de jogadores, verifica se em c existe algum fantasma no modo Alive
--
temGhostAlive :: Coords -> [Player] -> Bool
temGhostAlive c [] = False
temGhostAlive c (g@(Ghost (GhoState s Alive)):ps) = (getPlayerCoords g == c) || temGhostAlive c ps
temGhostAlive c (p:ps) = temGhostAlive c ps

-- | Dada uma posição c e uma lista de jogadores, verifica se em c existe algum fantasma
--
temGhost :: Coords -> [Player] -> Bool
temGhost c [] = False
temGhost c (g@(Ghost gs):ps) = (getPlayerCoords g == c) || temGhost c ps
temGhost c (p:ps) = temGhost c ps

-- | Dado um número x, acrescenta a cada par esse número (tranformando o par
-- num triplo. Além disso,
-- consoante a orientação dada, muda ou não as orientações dos elementos da lista
--
mudaPossib :: Double -> Orientation -> [(Coords, Orientation)] -> [(Coords,Double,Orientation)]
mudaPossib x Null = map (\(c,o)-> (c,x,o))
mudaPossib x o = map (\(c,d)-> (c,x,o))


{- | Com o objetivo de descobrir para que direção o pacman deve ir para se aproximar de uma comida, é utilizado
novamente o algoritmo Breadth-First

-}
comidaMaisProx :: [(Coords, Orientation)] -> Maze -> Orientation -> Orientation
comidaMaisProx (((x,y),o):t) m d = let p = ((m!!x)!!y) in
                                   if (p == Food Little || p==Food Big ) then o
                                   else if (o==Null)
                                        then comidaMaisProx (t++pM) m' d
                                        else comidaMaisProx (t ++ (mudarOrient o pM)) m' d
                                 where m' = replaceElemInMaze (x,y) Wall m
                                       pM = possibilidadesMov m (x,y) d

{- | Recebe uma lista com as orientações que levarão o Pacman a encontrar um fantasma,
com a respetiva distância a este. Com esta informação, procura a melhor forma de evitar os fantasmas.
Se dentro das possibilidades de movimento existirem orientações que não estão
na lista dada, é escolhida uma delas aleatoriamente.
Caso contrário, escolhe-se aquela cujo fantasma está mais longe

__Obs:__ Decidimos que a orientação escolhida, quando existe possibilidade de escolha, é
aleatória visto que quando experimentámos escolhendo a primeira, o pacman, numa dada altura
do jogo limitava-se a fugir dos fantasmas, dando voltas ao labirinto até eventualmente morrer

-}
decideOrientacao :: [(Double, Orientation)] -> Coords -> Maze -> Orientation -> Orientation
decideOrientacao l (x,y) m d = let possib = possibilidadesMov m (x,y) d
                                   lODif = orientDiferentes possib l
                               in
                               case lODif of
                                [] -> maisLonge l (0,Null)
                                l -> unsafePerformIO $ getRandomElem l

getRandomElem :: [Orientation] -> IO Orientation
getRandomElem l = do x <- randomRIO (0, (length l)-1)
                     return (l!!x)

{- | Retorna todos as orientações que aparecem na primeira lista, mas não aparecem na segunda

-}
orientDiferentes :: [(Coords,Orientation)] -> [(Double,Orientation)] -> [Orientation]
orientDiferentes l1 l2 = concat (map (temODif l2) l1)
     where temODif [] (_,o) = [o]
           temODif ((_,o1):t) (x,o2) = if (o1==o2) then []
                                       else temODif t (x,o2)

{- | Retorna a orientação cujo o número correspondente é o maior

-}
maisLonge :: [(Double,Orientation)] -> (Double,Orientation) -> Orientation
maisLonge [] (x,o) = o
maisLonge ((a,b):t) (c,d) = if (a>c) then maisLonge t (a,b)
                            else maisLonge t (c,d)

{- | Com o objetivo de retornar a melhor jogada que o Pacman pode fazer quando está no modo Mega,
primeiro verifica-se se existe algum fantasma Dead cujo comprimento de caminho mais curto seja atingivel no
tempo que resta no modo Mega e menor que a distGhost (definida nos Types). 
Se existir ele procura aproximar-se dele, se não terá um comportamento semelhante ao pacman
quando está no modo Normal.

-}
botMega :: Player -> State -> Play
botMega p (State m ps l) = let cP = getPlayerCoords p
                               gs = ghostMaisProx [(cP,0,Null)] m ps (getMoves p) pO 
                               pid = getPlayerID p
                               pO = getPlayerOrientation p
                            in
                            case gs of
                              Nothing -> botNormal p (State m ps l)
                              Just x -> Move pid x

{- | Descobre o número de máximo de jogadas que queremos analisar consoante o tempo Mega que o pacman ainda tem.
Se der para fazer um número de jogadas igual ou superior à distGhost, paenas devolve a distGhost. Se não devolve
o número de jogadas que consegue efetuar

-}
getMoves :: Player -> Double
getMoves (Pacman (PacState a b c d)) = let dT = fromInteger defaultDelayTime in
                                       if (b<= (distGhost * (dT/1000)))
                                       then b/(dT/1000)
                                       else distGhost


{- | Caso exista um fantasma no modo Dead alcançavel com um número restrito de jogadas (moves)
devolve o orientação que leva o Pacman a este. Caso contrário, devolve Nothing.

-}
ghostMaisProx :: [(Coords,Double,Orientation)] -> Maze -> [Player] -> Double -> Orientation -> Maybe Orientation
ghostMaisProx ((c,n,o):t) _ _ moves _ | moves<n = Nothing
ghostMaisProx ((c,n,o):t) m ps moves d = if (temGhostAlive c ps) then Nothing
                                         else if (temGhost c ps) then (Just o)
                                              else ghostMaisProx (t ++ (mudaPossib (n+1) o pM)) m' ps moves d
                                       where pM = possibilidadesMov m c d
                                             m' = replaceElemInMaze c Wall m