module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6
import FileUtils



loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/2.txt") 2 0 0 0 defaultDelayTime )


updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer key (Manager (State m ps l) pid step bf delt del)
      = Manager (State m (mudaDirecao key ps pid) l) pid step bf delt del

mudaDirecao :: Key -> [Player] -> Int -> [Player]
mudaDirecao key (p:ps) pid = if (getPlayerID p /= pid) then p: mudaDirecao key ps pid
                             else case key of
                              KeyUpArrow -> mudaOrientacao p U: ps
                              KeyDownArrow -> mudaOrientacao p D: ps
                              KeyLeftArrow -> mudaOrientacao p L:ps
                              KeyRightArrow -> mudaOrientacao p R:ps

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man@(Manager s pid step bf delt del ) =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show s
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager s pid step bf delt del )
      = (Manager s pid step bf (now - bf) del )


resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager s pid step bf delt del )
      = (Manager s pid step now 0 del ) 

nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager s pid step bf delt del )
      =  let botPac = bot 2 s in
         case botPac of
          Nothing ->  Manager (passTime step s) pid (step+1) now 0 del 
          Just p -> Manager (play p (passTime step s)) pid (step+1) now 0 del
       


loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorWhite ColorBlack 10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if (morreuPacman s) then return ()
  else
       if ( delt > del )
       then loop w $ nextFrame now man
       else do
              ev <- getEvent w $ Just 0
              case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)


-- | Determina se o pacman se encontra no modo Dying
--
morreuPacman :: State -> Bool
morreuPacman (State m [] l) = False
morreuPacman (State m (p:ps) l) = case p of
                                   Ghost gs -> morreuPacman (State m ps l)
                                   Pacman (PacState _ _ _ Dying) -> True
                                   Pacman ps -> False

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

