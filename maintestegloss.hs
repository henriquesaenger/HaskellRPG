import qualified Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type World = (Float, Float)

pikaimg = (Graphics.Gloss.Game.jpg "imgs/pikaback.jpg")

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
  windowDisplay = InWindow "Window" (1024, 600) (10, 10)

  simulationRate :: Int
  simulationRate = 20

  initialModel :: World
  initialModel = (0,0)

  drawingFunc :: World -> Picture
  drawingFunc (x, y) = translate x y pikaimg

  inputHandler :: Event -> World -> World
  inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
  inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
  inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
  inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
  inputHandler _ w = w

  updateFunc :: Float -> World -> World
  updateFunc _ (x, y) = (towardCenter x, towardCenter y)
   where
    towardCenter :: Float -> Float
    towardCenter c = if abs c < 0.25
     then 0
     else if c > 0
      then c - 0.25
      else c + 0.25