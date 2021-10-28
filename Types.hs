module Types where

import Data.List

data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState= PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState = GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)

data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 

instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


coloredString :: String -> Color -> String
coloredString x y = x {-
     | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
     | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
     | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
     | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
     | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
     | otherwise =  "\x1b[0m" ++ x 
-}

placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
  
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

getPlayerVelocity :: Player -> Double
getPlayerVelocity (Pacman (PacState (x,y,z,t,h,l) b c d )) = z
getPlayerVelocity (Ghost (GhoState (x,y,z,t,h,l) b )) = z

-- | Dá o número de linhas de um labirinto
--
linhas :: Maze -> Int
linhas l = length l 

-- | Dá o número de colunas de um labirinto
--
colunas :: Maze -> Int
colunas (x:xs) = length x


-- | Descobre a posição para a qual o jogador se irá movimentar, de acordo com a orientação
--
-- __Obs:__ Se o jogador entra num túnel, ou seja, se se encontrar numa posição (x,0)
-- e se mover para a esquerda ou se se encontrar na última coluna do do labirinto e
-- se mover para a direita, é transportado para o lado oposto do labirinto
--
proximaPosicao :: Maze -> Coords -> Orientation -> Coords
proximaPosicao (h:t) (x,0) L = (x, length h -1)
proximaPosicao m (x,y) L     = (x,y-1)
proximaPosicao m (x,y) U     = (x-1,y)
proximaPosicao m (x,y) D     = (x+1,y)
proximaPosicao (h:t) (x,y) R = if (y== length h -1) then (x,0)
                               else (x,y+1)
proximaPosicao m c Null = c

-- | Altera a orientação de um jogador
--
mudaOrientacao :: Player -> Orientation -> Player
mudaOrientacao (Pacman (PacState (x,y,z,t,h,l) q c d )) o
          = (Pacman (PacState (x,y,z,o,h,l) q c d ))
mudaOrientacao (Ghost (GhoState (x,y,z,t,h,l) q )) o
          = (Ghost (GhoState (x,y,z,o,h,l) q ))                             

-- | Dada uma orientação retorna aquele correspondente a rodar para a direita
turnRight :: Orientation -> Orientation
turnRight L = U
turnRight U = R
turnRight R = D
turnRight D = L
turnRight Null = Null

-- | Dada uma lista de jogadores e um inteiro x, devolve o jogador com cujo id é igual a x 
encontraPlayer :: [Player] -> Int -> Player
encontraPlayer (p:ps) x = if (getPlayerID p == x) then p
                          else encontraPlayer ps x

-- | Distância que consideramos para analisar o que rodeia o Pacman.
-- 
-- __Obs: __Inicialmente tínhamos considerado uma distância maior, no entanto,
-- o ghci demorava muito a processar,
-- tirando a fluidez ao jogo. Decidimos então diminir para 5, após testar as várias opções.
--
distGhost :: Double                          
distGhost = 5

mz3 = [[Wall, Food Big, Food Big, Food Little], [Wall, Wall, Empty, Food Little], [Wall,Wall,Wall,Wall],[Wall, Wall, Empty, Food Little]]
mz4 = [[Wall, Food Big, Food Big, Food Little],[Wall, Food Big, Food Big, Food Little],[Wall, Food Big, Food Big, Food Little]]
mz5 = [[Wall, Food Big, Food Big, Food Little], [Wall, Wall, Empty, Food Little],[Wall, Wall, Empty, Food Little],[Wall, Food Big, Food Big, Food Little]]
mz6 = [[Wall, Food Big, Food Big, Food Little], [Wall, Wall, Empty, Food Little],[Wall, Food Big, Food Big, Food Little], [Wall, Wall, Empty, Food Little]]
pac1 = Pacman (PacState (0,(6,3),1,L,10,2) 0 Open Normal)
pac2 = Pacman (PacState (0,(4,0),1,U,10,2) 0.2 Open Mega)
pac3 = Pacman (PacState (0,(6,3),1,U,10,2) 0 Open Normal)
pac4 = Pacman (PacState (0,(6,3),1,D,10,2) 0 Open Normal)
pac5 = Pacman (PacState (0,(6,3),1,L,10,2) 0 Open Normal)
pac6 = Pacman (PacState (0,(6,3),1,R,10,0) 0 Open Normal)
gh1 = Ghost (GhoState (1,(6,4),1,L,0,2) Alive)
gh2 = Ghost (GhoState (1,(2,3),1,L,0,2) Dead)
gh3 = Ghost (GhoState (2,(6,2),1,L,0,2) Dead)
