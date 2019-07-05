import qualified Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data World = Game 
 { jonas::(Float,Float)--posicao jonas
 , sbp::(Float, Float)--posicao sbp
 , matamoscas::(Float, Float)
 , matamoscasstate::(Float)--para alternar imagem o mata moscas
 , raid::(Float, Float)--
 , aranha::(Float, Float)--
 , bomba::(Float, Float)--bomba atomica para salvar o mundo
 , wjonas::(Float, Float)--jonas vitorioso
 , djonas::(Float, Float)--jonas morto
 , dijonas::(Float, Float)--jonas morto por inseticida
 , upButton::Bool -- botao pressionado
 , downButton::Bool
 , rightButton::Bool
 , leftButton::Bool
 , paredes::[([Float],[Float])]--paredes da fase
 , frame::Float
 } deriving Show

imgjonas = (Graphics.Gloss.Game.png "imgs/jonas.png")
imgsbp = (Graphics.Gloss.Game.png "imgs/sbpt.png")
imgmmoscas1 = (Graphics.Gloss.Game.png "imgs/matamoscas1t.png")
imgmmoscas2 = (Graphics.Gloss.Game.png "imgs/matamoscas2t.png")
imgraid = (Graphics.Gloss.Game.png "imgs/raidt.png")
imgaranha = (Graphics.Gloss.Game.png "imgs/aranhat.png")
imgbomba = (Graphics.Gloss.Game.png "imgs/bombaatomicat.png")
imgwjonas = (Graphics.Gloss.Game.png "imgs/winnerjonast.png")
imgdjonas = (Graphics.Gloss.Game.png "imgs/deadjonast.png")
imgdijonas = (Graphics.Gloss.Game.png "imgs/deadjonasinseticidat.png")
imginsetatk = (Graphics.Gloss.Game.png "imgs/matamoscas2t.png")
imgspray = (Graphics.Gloss.Game.png "imgs/esporradat.png")

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
  initialModel = Game 
   {jonas = (-490,-280)
   , sbp = (-100, 100)
   , upButton = False
   , matamoscas = (100,100)
   , matamoscasstate = 0
   , raid = (-80, 200)
   , aranha = (-60, -200)
   , bomba = (480, 250)
   , wjonas = (1000, 1000)
   , djonas = (1000, 1000)
   , dijonas = (1000,1000)
   , downButton = False
   , rightButton = False
   , leftButton = False
   , paredes = --quando chegar no 0, sempre iniciar uma nova parede
    [(map fromIntegral [0,1..40],map fromIntegral [-250,-251..(-280)])
    ,(map fromIntegral [-400,-401..(-500)],map fromIntegral [0,1..40])
    ,(map fromIntegral [0,1..50],map fromIntegral [0,1..50])
    ,(map fromIntegral [0,-1..(-50)],map fromIntegral [0,-1..(-50)])
    ]
   , frame = 0
   }

  drawingFunc :: World -> Picture
  drawingFunc w = pictures 
   [pictures (let (x,y,ox,oy) = ((map leftArg (map rightArg (map getRekt (paredes w))))
   	                              , (map rightArg (map rightArg (map getRekt (paredes w))))
   	                              , (map leftArg (map leftArg (map getRekt (paredes w))))
   	                              , (map rightArg (map leftArg (map getRekt (paredes w))))) 
                in map myTranslate (zipao (map calculoZipao (zip2 ox x)) (map calculoZipao (zip2 oy y)) (map myRecSolid (zip2 x y))))
   , translate (leftArg (sbp w)) (rightArg (sbp w)) imgsbp
   , translate (leftArg (raid w)) (rightArg (raid w)) imgraid
   , translate ((leftArg (matamoscas w))+1000.0*(matamoscasstate w)) ((rightArg(matamoscas w))+1000.0*(matamoscasstate w)) imgmmoscas1
   , translate ((leftArg (matamoscas w))+1000.0-(1000.0*(matamoscasstate w))) ((rightArg(matamoscas w))+1000.0-(1000.0*(matamoscasstate w))) imgmmoscas2
   , translate (leftArg (aranha w)) (rightArg (aranha w)) imgaranha
   , translate (leftArg (bomba w)) (rightArg (bomba w)) imgbomba
   , translate (leftArg (jonas w)) (rightArg (jonas w)) imgjonas
   , translate ((leftArg (sbp w))-25) ((rightArg (sbp w))+10) imgspray
   , translate ((leftArg (raid w))-25) ((rightArg (raid w))+10) imgspray
   , translate (leftArg (wjonas w)) (rightArg (wjonas w)) imgwjonas
   , translate (leftArg (djonas w)) (rightArg (djonas w)) imgdjonas
   , translate (leftArg (dijonas w)) (rightArg (dijonas w)) imgdijonas
   ]

  inputHandler :: Event -> World -> World
  inputHandler (EventKey (SpecialKey KeyUp) Down _ _) w = w{upButton = True}
  inputHandler (EventKey (SpecialKey KeyDown) Down _ _) w = w{downButton = True}
  inputHandler (EventKey (SpecialKey KeyRight) Down _ _) w = w{rightButton = True}
  inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) w = w{leftButton = True}
  inputHandler (EventKey (SpecialKey KeyUp) Up _ _) w = w{upButton = False}
  inputHandler (EventKey (SpecialKey KeyDown) Up _ _) w = w{downButton = False}
  inputHandler (EventKey (SpecialKey KeyRight) Up _ _) w = w{rightButton = False}
  inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) w = w{leftButton = False}
  inputHandler (EventKey (Char 'w') Down _ _) w = w{upButton = True}
  inputHandler (EventKey (Char 'w') Up _ _) w = w{upButton = False}
  inputHandler (EventKey (Char 's') Down _ _) w = w{downButton = True}
  inputHandler (EventKey (Char 's') Up _ _) w = w{downButton = False}
  inputHandler (EventKey (Char 'd') Down _ _) w = w{rightButton = True}
  inputHandler (EventKey (Char 'd') Up _ _) w = w{rightButton = False}
  inputHandler (EventKey (Char 'a') Down _ _) w = w{leftButton = True}
  inputHandler (EventKey (Char 'a') Up _ _) w = w{leftButton = False}
  --inputHandler (EventKey (Char 'r') Down _ _) w = (initialModel w)
  inputHandler _ w = w

  updateFunc :: Float -> World -> World
  updateFunc _ w = if (colisao (jonas w) (aranha w))||(colisao (jonas w) (matamoscas w))||(colisao (jonas w) (raid w))||(colisao (jonas w) (sbp w)) then w{jonas = (1000,1000), sbp = (1000,1000), djonas = (0,0)}
   else if (colisaoi (jonas w) (sbp w))||(colisaoi (jonas w) (raid w)) then w{jonas = (1000,1000), sbp = (1040, 1000), dijonas = (0,0)}
   else (changeMMState.movementCompute.getWorldFrame) w

getRekt::([a],[a]) -> ((a,a),(a,a))
getRekt (a, b) = (((primeiro a),(primeiro b)),((primeiro(reverse a)), (primeiro(reverse b))))
 where
  primeiro::[a] -> a
  primeiro (a:x) = a

myRecSolid::(Float,Float) -> Picture
myRecSolid (x,y) = rectangleSolid x y

zip2::[a] -> [a] -> [(a,a)]
zip2 [] [] = []
zip2 (x:a) (y:b) = (x,y):(zip2 a b)

calculoZipao::(Float, Float) -> Float
calculoZipao (ox,x) = ox+(x/2)

myTranslate::(Float, Float, Picture) -> Picture
myTranslate (a,b,c) = translate a b c

zipao::[Float] -> [Float] -> [Picture] -> [(Float, Float, Picture)]
zipao [] _ _ = []
zipao _ [] _ = []
zipao _ _ [] = []
zipao (x:a) (y:b) (z:c) = (x,y,z):(zipao a b c)

movementCompute::World->World
movementCompute w = w{jonas = (x',y')}
 where
  (x,y) = jonas w
  y' = if (upButton w) then rightArg(colisaoParedes w 1) else if (downButton w) then rightArg(colisaoParedes w 2) else y
  x' = if (rightButton w) then leftArg(colisaoParedes w 3) else if (leftButton w) then leftArg(colisaoParedes w 4) else x

getWorldFrame::World -> World
getWorldFrame w = w{frame = ((frame w)+1)}

changeMMState::World -> World
changeMMState w = if ((modFloat (frame w) 120)>60) then w{matamoscasstate = 1} else w{matamoscasstate = 0}

modFloat::Float -> Float -> Float
modFloat a b = fromIntegral((round a) `mod` (round b))

colisao::(Float,Float)->(Float,Float)->Bool
colisao (x1,y1) (x2,y2) = 
 if (x1<=x2 && (x1+10)>=x2 && y1<=y2 && (y1+10)>=y2)||(x1<=x2 && (x1+10)>=x2 && y1>=y2 && (y1-10)<=y2)|| ---baixo esquerda, cima esquerda
 (x1>=x2 && (x1-10)<=x2 && y1<=y2 && (y1+10)>=y2)||(x1>=x2 && (x1-10)<=x2 && y1>=y2 && (y1-10)<=y2) ---baixo direita, cima direita
 then True else False

colisaoi::(Float,Float)->(Float,Float)->Bool
colisaoi (x1,y1) (x2,y2) = 
 if (x1<=(x2-40) && (x1+10)>=(x2-40) && y1<=(y2+10) && (y1+10)>=(y2+10))||(x1<=(x2-40) && (x1+10)>=(x2-40) && y1>=(y2+10) && (y1-10)<=(y2+10))|| ---baixo esquerda, cima esquerda
 (x1>=(x2-40) && (x1-10)<=(x2-10) && y1<=(y2+10) && (y1+10)>=(y2+10))||(x1>=(x2-40) && (x1-10)<=(x2-10) && y1>=(y2+10) && (y1-10)<=(y2+10)) ---baixo direita, cima direita
 then True else False

colisaoParedes::World -> Int -> (Float, Float)
colisaoParedes w 1 = (x,y') --up
 where
  (x,y) = jonas w
  p = paredes w
  y' = if y<(280)&& not (parede (x,y+10) p) then y+1 else y
colisaoParedes w 2 = (x,y') --down
 where
  (x,y) = jonas w
  p = paredes w
  y' = if y>(-280)&& not (parede (x,y-10) p) then y-1 else y
colisaoParedes w 3 = (x',y) --right
 where
  (x,y) = jonas w
  p = paredes w
  x' = if x<(490)&& not (parede (x+10,y) p) then x+1 else x
colisaoParedes w 4 = (x',y) --left
 where
  (x,y) = jonas w
  p = paredes w
  x' = if x>(-490)&& not (parede (x-10,y) p) then x-1 else x

moveJonas::World -> Int -> World
moveJonas w 1 = w {jonas = (x,y')}
 where
  (x,y) = jonas w
  p = paredes w
  y' = if y<(280)&& not (parede (x,y+10) p) then (y+10) else y
moveJonas w 2 = w {jonas = (x,y')}
 where
  (x,y) = jonas w
  p = paredes w
  y' = if y>(-280)&& not (parede (x,y-10) p) then y-10 else y
moveJonas w 3 = w {jonas = (x',y)}
 where
  (x,y) = jonas w
  p = paredes w
  x' = if x<(490)&& not (parede (x+10,y) p) then x+10 else x
moveJonas w 4 = w {jonas = (x',y)}
 where
  (x,y) = jonas w
  p = paredes w
  x' = if x>(-490)&& not (parede (x-10,y) p) then x-10 else x

parede::(Float, Float) -> [([Float],[Float])] -> Bool
parede (x,y) [] = False
parede (x,y) ((a,b):l) = if (pertence x a) && (pertence y b) then True else parede (x,y) (l)

pertence::Eq a => a -> [a] -> Bool
pertence item [] = False
pertence item (elem:lista) = if item == elem then True else pertence item lista

leftArg::(a,a)->a
leftArg (x,y) = x

rightArg::(a,a)->a
rightArg (x,y) = y