import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Model = (Float, Float)

main::IO()
main = simulate
 windowDisplay
 white
 simulationRate
 initialModel
 drawingFunc
 updateFunc
 where

  windowDisplay :: Display
  windowDisplay = InWindow "Window" (1024, 600) (10, 10)

  simulationRate :: Int
  simulationRate = 20

  initialModel :: Model
  initialModel = (0,0)

  drawingFunc :: Model -> Picture
  drawingFunc (theta, dtheta) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]

  updateFunc :: ViewPort -> Float -> Model -> Model
  updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))