import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Complex = C {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
  deriving (Show, Eq)

instance Num Complex where
  fromInteger n = C (fromIntegral n) 0.0
  (C x y) * (C z t) = C (z*x-y*t) (y*z + x*t)
  (C x y) + (C z t) = C (x+z) (y+t)
  abs (C x y)       = C (sqrt $ x*x + y*y) 0
  signum (C x _)    = C (signum x) 0.0

complex :: Float -> Float -> Complex
complex = C

real :: Complex -> Float
real (C x _) = x

imag :: Complex -> Float
imag (C _ y) = y

magnitude :: Complex -> Float
magnitude = real . abs

main :: IO ()
main = do
  -- Initialize GLUT
  _ <- getArgsAndInitialize

  -- Use double-buffered display mode
  initialDisplayMode $= [DoubleBuffered]

  -- Specify window title
  _ <- createWindow "Hullo World"

  -- Call 'display' each time we need to update
  displayCallback $= display

  -- Enter the main loop
  mainLoop

display :: IO ()
display = do
  -- Make the window black
  clear [ColorBuffer]

  -- Reset any transformation
  loadIdentity

  preservingMatrix drawSomething

  -- Refresh screen
  swapBuffers

drawSomething :: IO ()
drawSomething = do
  renderPrimitive Points $ do -- LineLoop $ do
    mapM_ drawColoredPoint allPoints
  where
    drawColoredPoint (x, y, c) = do
      color c
      vertex $ Vertex3 x y 0

height :: GLfloat
height = 320

width :: GLfloat
width  = 320

allPoints :: [(GLfloat, GLfloat, Color3 GLfloat)]
-- allPoints = positivePoints ++
--               map (\(x,y,c) -> (x,-y,c)) (reverse positivePoints)
allPoints = [ (x/width, y/height, colorFromValue $ mandel x y)
              | x <- [-width  .. width ]
              , y <- [-height .. height]]

positivePoints :: [(GLfloat, GLfloat, Color3 GLfloat)]
positivePoints = do
  x <- [-width .. width]
  let y = maxZeroIndex (mandel x) 0 height (log2 height)
  if y < 1
    then []
    else return (x/width, y/height, colorFromValue $ mandel x y)
  where log2 n = floor ((log n) / log 2)

maxZeroIndex f min max 0 = (min + max) / 2
maxZeroIndex f min max n = if f med /= 0
                             then maxZeroIndex f min med (n-1)
                             else maxZeroIndex f med max (n-1)
  where med = (min + max) / 2

colorFromValue :: Int -> Color3 GLfloat
colorFromValue n = Color3 (t n) (t (n+5)) (t (n+10))
  where
    t :: Int -> GLfloat
    t i = 0.5 + 0.5*cos(fromIntegral i / 10)

mandel :: GLfloat -> GLfloat -> Int
mandel x y = f (complex r i) 0 64
  where r = 2.0 * x / width
        i = 2.0 * y / height

f :: Complex -> Complex -> Int -> Int
f _ _ 0 = 0
f c z n = if magnitude z > 2 then n else f c ((z*z)+c) (n - 1)
