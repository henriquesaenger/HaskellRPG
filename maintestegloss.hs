import qualified Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type World = (
 (Float,Float),--posicao jonas
 (Float, Float)--posicao sbp
 )

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
  initialModel = ((-490,-280), (0, 0))

  drawingFunc :: World -> Picture
  drawingFunc ((xjonas, yjonas), (xsbp, ysbp)) = pictures 
   [translate xsbp ysbp imgsbp,
   translate xjonas yjonas imgjonas]

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
  updateFunc _ (jonas, (xsbp,ysbp)) = if (colisao jonas (xsbp, ysbp)) then ((1000,1000), (1000, 1000))
   else (jonas,(xsbp,ysbp))


colisao::(Float,Float)->(Float,Float)->Bool
colisao (x1,y1) (x2,y2) = 
 if (x1<=x2 && (x1+10)>=x2 && y1<=y2 && (y1+10)>=y2)||(x1<=x2 && (x1+10)>=x2 && y1>=y2 && (y1-10)<=y2)|| ---baixo esquerda, cima esquerda
 (x1>=x2 && (x1-10)<=x2 && y1<=y2 && (y1+10)>=y2)||(x1>=x2 && (x1-10)<=x2 && y1>=y2 && (y1-10)<=y2) ---baixo direita, cima direita
 then True else False

moveJonas::World -> Int -> World
moveJonas ((x,y), r) 1 = if y<(280)&& not (parede (x,y+10)) then ((x, y + 10),r) else ((x,y),r)
moveJonas ((x,y), r) 2 = if y>(-280)&& not (parede (x,y-10)) then ((x, y - 10),r) else ((x,y),r)
moveJonas ((x,y), r) 3 = if x<(490)&& not (parede (x+10,y)) then ((x+10, y),r) else ((x,y),r)
moveJonas ((x,y), r) 4 = if x>(-490)&& not (parede (x-10,y)) then ((x-10, y),r) else ((x,y),r)

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