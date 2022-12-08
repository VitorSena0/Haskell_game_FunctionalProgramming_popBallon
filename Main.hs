{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import System.Random
import qualified Data.Text as T
-- O T.pack serve para transformar uma string em um txt importando o Data.text 

data World = WConstructor {
  ballList :: [Point],
  dartPosition :: Point,
  velVerticalDart :: Double,
  randoList :: [Int],
  timer :: Double,
  score :: Int 
}

period = 2
velocidadeBola = 10 :: Double
g = -9
velH = 14
velVIni = 2

main = do 
         x:xs <- rndmLst 100
         activityOf  (WConstructor [(-4,-12)] (-10,0) 0 (x:xs) period 0) update visualization
    
--------------------- Game Elements Drawings And Background Draw ---------------------------------------------------------

road = colored (dark brown) (solidPolygon [(-1,0),(-4,-10),(4,-10),(1,0)])
ballon = scaled 0.8 1 (colored red (solidCircle 1)) & thickCurve 0.1 [(0,0),(0,-1),(-0.5,-1.5),(0.5,-2),(0,-2.5)]
controlsInfo1  = dilated 0.6 (lettering ("Press (↑ or w) to go up and (↓ or s) to go down"))
controlsInfo2  = dilated 0.6 (lettering ("Press 'Enter' for to throw the dart"))

cloud = dilated 0.7 (pictures [ translated x y  (colored  (translucent (brighter 1 white))(solidCircle 2)) | (x,y) <- zip [0,1,2,2.5,3,0.5,4,4,5,5.5,-1,6.3] 
                                                                                                                          [0,1,2,0,1.5,1.5,0,1,1.5,0,1.3,1]] )
                                                                                
cloudList = dilated 0.6 $ pictures [ translated x y (cloud) | (x,y) <- zip [0,-7,-3] [0,0,-1]  ]

dart = body & needle & taill
     where
       taill = solidPolygon [(-0.2,0),(-0.6,-0.3),(-0.6,0.3)]
       body = colored red (solidRectangle 1 0.25)
       needle = thickPolyline 0.05 [(0.5, 0), (0.7, 0) ]     
landscape =  gram & sun & sky
     where
       gram    = translated 0 (-5)( colored (dark green)( solidRectangle 20 (-10)))
       sky     = translated 0 5 (colored blue (solidRectangle 20 10))
       sun     = translated (-7.5) 7.5 (colored orange (thickCircle 0.3 2) & colored yellow (solidCircle 2))
home = roof & door & window & wall
     where
       roof   = translated 0 1.5 (colored (light red) (solidPolygon [(0,3),(-3.4,1.3),(3.4,1.3)]))
       window = translated 1.4 1 (colored (dark gray)(solidRectangle 1.3 1)) 
       door   =  translated (-0.5) 1(colored brown (solidRectangle 1 2))
       wall   = translated 0 1.5(colored (light green )(solidRectangle 5 3))
       
rndmLst :: Int -> IO [Int]
rndmLst n = sequence (Prelude.replicate n (randomRIO (-4, 7::Int)))

updateBalls t ps = [ (x, y + velocidadeBola * t) | (x, y) <- ps ]

visualization :: World -> Picture
visualization ( WConstructor _ _ _ [] _ _) = blank
visualization ( WConstructor [] _ _ _ _ _) = blank  

visualization ( WConstructor ((x,y):ps) (x2,y2) velYdart (n:ns) tm score) = 
  translated x2 y2 dart &
  translated x y ballon & 
  translated (-7) (-9) scoreboard & translated 4 (9) controlsInfo1 &
  translated 2.4 (8) controlsInfo2 &
  translated (-5) 7 cloudList & translated 0 0 (home & road & landscape) &
  visualization ( WConstructor ps (x2,y2) velYdart ns tm score)
   where 
      scoreboard = lettering ("Score: " <> T.pack (show (score))) & colored gray (solidRectangle 3.8 1.5)
    
update :: Event -> World ->World
update (KeyPress "Up")    (WConstructor ((x,y):ps) (x2,y2)   velY  (n:ns) time score)
    | x2 <= -10 =         (WConstructor ((x,y):ps) (x2,y2+1) velY  (n:ns) time score)    
update (KeyPress "Down")  (WConstructor ((x,y):ps) (x2,y2)   velY  (n:ns) time score)
    | x2 <= -10 =         (WConstructor ((x,y):ps) (x2,y2-1) velY  (n:ns) time score)
update (KeyPress "W")     (WConstructor ((x,y):ps) (x2,y2)   velY  (n:ns) time score)
    | x2 <= -10 =         (WConstructor ((x,y):ps) (x2,y2+1) velY  (n:ns) time score)    
update (KeyPress "S")     (WConstructor ((x,y):ps) (x2,y2)   velY  (n:ns) time score)
    | x2 <= -10 =         (WConstructor ((x,y):ps) (x2,y2-1) velY  (n:ns) time score) 
update (KeyPress "Enter") (WConstructor ((x,y):ps) (x2,y2)   velY  (n:ns) time score)
    | x2 <= -10 =         (WConstructor ((x,y):ps) (x2,y2)   6     (n:ns) time score)
    
    
update (TimePassing t)    (WConstructor ((x,y):ps)    (x2,y2) velY    (n:ns)    time    score)
   | newTime <= -1.6 =    (WConstructor ((x,-11):ballons) (x2,y2) velY    ns        period  score)
   | velY == 0   =        (WConstructor ballons    initialStage   0       (n:ns)    newTime score)
   | dartOutside =        (WConstructor ((x,y):ps) (-10,0) 0       (n:ns)    time    score) 
   | dartCollision =      (WConstructor dropBallon dartTrajectory velMRU   (n:ns) time   (score+1))     
   | otherwise =          (WConstructor ballons    dartTrajectory velMRU    (n:ns)    newTime score) 
  where
    dartCollision = x - 1 <= x2 && x2 <= x + 1 && y - 1 <= y2 && y2 <= y + 1  
    dartTrajectory = (x2 + velH*t,y2 + velY *t + 1/2*g*t^2)
    newTime = time - t
    dartOutside = x2 >= 10 || y2 <= -10 
    dropBallon = drop 1 ((x,y):ps)
    ballons = updateBalls t ((fromIntegral n ,y) : ps)
    initialStage = (x2 + velY*velH*t ,y2 + velY *t + 1/2*0*t^2)
    velMRU = velY + g*t
update _ w                          = w
