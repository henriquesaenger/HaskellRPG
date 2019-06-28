import qualified Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data World = Game {
 jonas::(Float,Float),--posicao jonas
 sbp::(Float, Float)--posicao sbp
 } deriving Show

imgjonas = (Graphics.Gloss.Game.png "imgs/jonas.png")
imgsbp = (Graphics.Gloss.Game.png "imgs/sbpt.png")

main::IO()
main = play
 windowDisplay
 white
 simulationRate
 initialModel
 drawingFunc
 inputHandler
 updateFunc
 where

  windowDisplay :: Display
  windowDisplay = InWindow "Jonas saving the world!" (1024, 600) (10, 10)

  simulationRate :: Int
  simulationRate = 60

  initialModel :: World
  initialModel = Game {jonas = (-490,-280), sbp = (0, 0)}

  drawingFunc :: World -> Picture
  drawingFunc w = pictures 
   [translate (leftArg (sbp w)) (rightArg (sbp w)) imgsbp,
   translate (leftArg (jonas w)) (rightArg (jonas w)) imgjonas]

  inputHandler :: Event -> World -> World
  inputHandler (EventKey (SpecialKey KeyUp) Down _ _) w = moveJonas w 1
  inputHandler (EventKey (SpecialKey KeyDown) Down _ _) w = moveJonas w 2
  inputHandler (EventKey (SpecialKey KeyRight) Down _ _) w = moveJonas w 3
  inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) w = moveJonas w 4
  inputHandler (EventKey (Char 'w') Down _ _) w = moveJonas w 1
  inputHandler (EventKey (Char 's') Down _ _) w = moveJonas w 2
  inputHandler (EventKey (Char 'd') Down _ _) w = moveJonas w 3
  inputHandler (EventKey (Char 'a') Down _ _) w = moveJonas w 4
  inputHandler _ w = w

  updateFunc :: Float -> World -> World
  updateFunc _ w = if (colisao (jonas w) (sbp w)) then w{jonas = (1000,1000), sbp = (1000,1000)}
   else w


colisao::(Float,Float)->(Float,Float)->Bool
colisao (x1,y1) (x2,y2) = 
 if (x1<=x2 && (x1+10)>=x2 && y1<=y2 && (y1+10)>=y2)||(x1<=x2 && (x1+10)>=x2 && y1>=y2 && (y1-10)<=y2)|| ---baixo esquerda, cima esquerda
 (x1>=x2 && (x1-10)<=x2 && y1<=y2 && (y1+10)>=y2)||(x1>=x2 && (x1-10)<=x2 && y1>=y2 && (y1-10)<=y2) ---baixo direita, cima direita
 then True else False

moveJonas::World -> Int -> World
moveJonas w 1 = w {jonas = (x,y')}
 where
  (x,y) = jonas w
  y' = if y<(280)&& not (parede (x,y+10)) then (y+10) else y
moveJonas w 2 = w {jonas = (x,y')}
 where
  (x,y) = jonas w
  y' = if y>(-280)&& not (parede (x,y-10)) then y-10 else y
moveJonas w 3 = w {jonas = (x',y)}
 where
  (x,y) = jonas w
  x' = if x<(490)&& not (parede (x+10,y)) then x+10 else x
moveJonas w 4 = w {jonas = (x',y)}
 where
  (x,y) = jonas w
  x' = if x>(-490)&& not (parede (x-10,y)) then x-10 else x

parede::(Float, Float) -> Bool
parede (0 ,(-280)) = True
parede (0 ,(-270)) = True
parede (0 ,(-260)) = True
parede (0 ,(-250)) = True
parede (0 ,(-240)) = True
parede (0 ,(-230)) = True
parede (0 ,(-220)) = True
parede (10 ,(-280)) = True
parede (10 ,(-270)) = True
parede (10 ,(-260)) = True
parede (10 ,(-250)) = True
parede (10 ,(-240)) = True
parede (10 ,(-230)) = True
parede (10 ,(-220)) = True
parede (_,_) = False


leftArg::(a,a)->a
leftArg (x,y) = x

rightArg::(a,a)->a
rightArg (x,y) = y