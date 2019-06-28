import qualified Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type World = (Float, Float)

pikaimg = (Graphics.Gloss.Game.png "imgs/jonas.png")

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
  initialModel = (-490,-280)

  drawingFunc :: World -> Picture
  drawingFunc (x, y) = pictures [translate x y pikaimg, translate x (y+50) pikaimg]

  inputHandler :: Event -> World -> World
  inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = if y<(280) then (x, y + 10) else (x,y)
  inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = if y>(-280) then (x, y - 10) else (x,y)
  inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = if x<(490) then (x + 10, y) else (x,y)
  inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = if x>(-490) then (x - 10, y) else (x,y)
  inputHandler (EventKey (Char 'w') Down _ _) (x, y) = if y<(280) then (x, y + 10) else (x,y)
  inputHandler (EventKey (Char 's') Down _ _) (x, y) = if y>(-280) then (x, y - 10) else (x,y)
  inputHandler (EventKey (Char 'd') Down _ _) (x, y) = if x<(490) then (x + 10, y) else (x,y)
  inputHandler (EventKey (Char 'a') Down _ _) (x, y) = if x>(-490) then (x - 10, y) else (x,y)
  inputHandler _ w = w

  updateFunc :: Float -> World -> World
  updateFunc _ (x, y) = (x, y)