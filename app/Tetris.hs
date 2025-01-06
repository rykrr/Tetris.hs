module Tetris
  ( Tetris (activeTetrimino, nextTetrimino, interrupt, matrix, score),
    Tetrimino (..),
    Interrupt (..),
    Rotation (..),
    Colour (..),
    colours,
    Block (..),
    Shape (..),
    tetrinit,
    tetristep,
    clearInterrupt,
    rotate,
    moveDown,
    moveLeft,
    moveRight,
  )
where

import Data.Maybe
import qualified Data.Vector as V
import GHC.Integer (divInteger)
import System.Random

(//) = divInteger

(@@) = (V.!)

data Shape = I | J | L | O | S | T | Z deriving (Ord, Eq, Enum)

data Colour = Cyan | Blue | Orange | Yellow | Green | Magenta | Red | White deriving (Eq, Ord, Enum)

data Block = Block Colour deriving (Eq, Ord)

data Rotation = Clockwise | CounterClockwise

data RotationValue = R0 | R90 | R180 | R270

data TimerMode = Gravity | Lock

data Interrupt = GameOver | RestartTimer | StartGravityTimer | StartLockTimer

shapes :: [Shape]
shapes = [I ..]

colours :: [Colour]
colours = [Cyan ..]

shapeToColour :: Shape -> Colour
shapeToColour = fromJust . flip lookup (zip shapes colours)

rotationValue Clockwise R0 = R90
rotationValue Clockwise R90 = R180
rotationValue Clockwise R180 = R270
rotationValue Clockwise R270 = R0
rotationValue CounterClockwise R0 = R270
rotationValue CounterClockwise R90 = R0
rotationValue CounterClockwise R180 = R90
rotationValue CounterClockwise R270 = R180

data Tetrimino = Tetrimino
  { shape :: Shape,
    colour :: Colour,
    points :: Points,
    size :: Integer,
    posX :: Integer,
    posY :: Integer,
    rotation :: RotationValue
  }

newTetrimino :: Integer -> Shape -> Tetrimino
newTetrimino w s =
  Tetrimino
    { shape = s,
      colour = shapeToColour s,
      points = points' s,
      size = matrixSize' s,
      posY = 0,
      posX = (w // 2) - (matrixSize' s // 2) - if matrixSize' s == 3 then 1 else 0,
      rotation = R0
    }
  where
    points' :: Shape -> Points
    points' O = [(0, 0), (0, 1), (1, 0), (1, 1)]
    points' I = [(1, 0), (1, 1), (1, 2), (1, 3)]
    points' J = [(0, 0), (1, 0), (1, 1), (1, 2)]
    points' L = [(1, 0), (1, 1), (1, 2), (0, 2)]
    points' T = [(0, 1), (1, 0), (1, 1), (1, 2)]
    points' S = [(0, 1), (0, 2), (1, 0), (1, 1)]
    points' Z = [(0, 0), (0, 1), (1, 1), (1, 2)]

    matrixSize' :: Shape -> Integer
    matrixSize' O = 2
    matrixSize' I = 4
    matrixSize' _ = 3

type Matrix a = V.Vector (V.Vector a)

type Points = [(Integer, Integer)]

data Tetris = Tetris
  { width :: Integer,
    height :: Integer,
    score :: Integer,
    matrix :: Matrix (Maybe Block),
    activeTetrimino :: Tetrimino,
    nextTetrimino :: Tetrimino,
    interrupt :: Maybe Interrupt,
    randgen :: StdGen,
    mode :: TimerMode
  }

randomTetrimino :: Integer -> StdGen -> (Tetrimino, StdGen)
randomTetrimino w g = do
  let n = length shapes
  let (r, g') = randomR (0, n - 1) g
  let s = newTetrimino w (shapes !! r)
  (s, g')

tetrinit :: StdGen -> Integer -> Integer -> Tetris
tetrinit g h w = do
  let (a, g') = randomTetrimino w g
  let (n, g'') = randomTetrimino w g'
  Tetris
    { height = h,
      width = w,
      score = 0,
      matrix = V.replicate (fromIntegral h + 0) $ V.replicate (fromIntegral w) Nothing,
      activeTetrimino = a,
      nextTetrimino = n,
      interrupt = Nothing,
      randgen = g'',
      mode = Gravity
    }

tetristep :: Tetris -> Tetris
tetristep tetris@(Tetris {width = w, matrix = m, activeTetrimino = a, nextTetrimino = n, randgen = g, score = s}) = do
  let (n', g') = randomTetrimino w g
  let tetris' = tetris {matrix = blit m a, activeTetrimino = n, nextTetrimino = n', randgen = g', mode = Gravity}
  let gameOver = not $ inBounds tetris' 0 0
  if gameOver then tetris' {interrupt = Just GameOver} else step tetris'
  where
    step tetris@(Tetris {height = h, matrix = m}) = do
      let newRows = V.filter (V.any isNothing) m
      let numRows = h - (fromIntegral $ V.length newRows)
      let emptyRow = V.replicate (fromIntegral w) Nothing
      let emptyRows = V.replicate (fromIntegral numRows) emptyRow
      tetris {matrix = (V.++) emptyRows newRows, score = s + numRows, interrupt = Just StartGravityTimer}

clearInterrupt tetris = tetris {interrupt = Nothing}

inBounds_ :: Tetris -> Integer -> Integer -> Points -> Bool
inBounds_ (Tetris {matrix = m, width = w, height = h, activeTetrimino = a}) dy dx p = do
  let (Tetrimino {posX = x, posY = y}) = a
  flip all p $ \(py, px) -> do
    let y' = y + py + dy
    let x' = x + px + dx
    0 <= y' && 0 <= x' && y' < h && x' < w && isNothing (m @@ fromIntegral y' @@ fromIntegral x')

inBounds :: Tetris -> Integer -> Integer -> Bool
inBounds tetris@(Tetris {matrix = m, width = w, height = h, activeTetrimino = a}) dy dx =
  inBounds_ tetris dy dx $ points a

inBoundsR :: Tetris -> (Points -> Points) -> Bool
inBoundsR tetris@(Tetris {matrix = m, width = w, height = h, activeTetrimino = a}) r =
  inBounds_ tetris 0 0 $ r $ points a

canMoveDown :: Tetris -> Bool
canMoveDown tetris = inBounds tetris 1 0

updateTetriminoPosX, updateTetriminoPosY :: (Integer -> Integer) -> Tetrimino -> Tetrimino
updateTetriminoPosX f t = t {posX = (f . posX) t}
updateTetriminoPosY f t = t {posY = (f . posY) t}

updateTetriminoPoints f t = t {points = (f . points) t}

updateActive :: Tetris -> (Tetrimino -> Tetrimino) -> Tetris
updateActive tetris@(Tetris {activeTetrimino = a, mode = m}) f = do
  let tetris' = tetris {activeTetrimino = f a}
  let (i', m') = setInterrupt (canMoveDown tetris') m

  tetris'
    { interrupt = Just i',
      mode = m'
    }
  where
    setInterrupt False Gravity = (StartLockTimer, Lock)
    setInterrupt True Lock = (StartGravityTimer, Lock)
    setInterrupt _ m = (RestartTimer, m)

moveDown, moveLeft, moveRight :: Tetris -> Tetris
moveDown tetris
  | canMoveDown tetris = updateActive tetris $ updateTetriminoPosY (+ 1)
  | otherwise = tetristep tetris

moveX offsetX tetris
  | inBounds tetris 0 offsetX = updateActive tetris $ updateTetriminoPosX (+ offsetX)
  | otherwise = tetris

moveLeft = moveX (-1)

moveRight = moveX 1

blit :: Matrix (Maybe Block) -> Tetrimino -> Matrix (Maybe Block)
blit m (Tetrimino {posY = y, posX = x, points = p, colour = c}) = flip V.imap m updateRow
  where
    updateRow :: Int -> V.Vector (Maybe Block) -> V.Vector (Maybe Block)
    updateRow i r = (V.//) r (toUpdates $ getRowPoints i)

    toUpdates :: [Int] -> [(Int, Maybe Block)]
    toUpdates = map (flip (,) . Just . Block $ c)

    getRowPoints :: Int -> [Int]
    getRowPoints i = map (fromIntegral . (+) x . snd) $ filter ((== i) . fromIntegral . (+) y . fst) p

rotate :: Rotation -> Tetris -> Tetris
rotate r tetris@(Tetris {activeTetrimino = a}) = do
  let t@(Tetrimino {posY = y, posX = x, shape = s, rotation = rv, points = p}) = rotateTetrimino r a

  case findKick p ((0, 0) : (kicks s r rv)) of
    Just (dy, dx) -> tetris {activeTetrimino = t {posY = y + dy, posX = x + dx}}
    Nothing -> tetris
  where
    findKick :: Points -> Points -> Maybe (Integer, Integer)
    findKick _ [] = Nothing
    findKick p ((dy, dx) : ds)
      | inBounds_ tetris dy dx p = Just (dy, dx)
      | otherwise = findKick p ds

    kicks :: Shape -> Rotation -> RotationValue -> Points
    kicks I Clockwise R0 = [(0, 1), (0, -2), (2, 1), (-1, -2)]
    kicks I Clockwise R90 = [(0, -2), (0, 1), (1, -2), (-2, 1)]
    kicks I Clockwise R180 = [(0, -1), (0, 2), (-2, -1), (1, 2)]
    kicks I Clockwise R270 = [(0, 2), (0, -1), (-1, 2), (2, -1)]
    kicks I CounterClockwise R0 = [(0, 2), (0, -1), (-1, 2), (2, -1)]
    kicks I CounterClockwise R90 = [(0, 1), (0, -2), (2, 1), (-1, -2)]
    kicks I CounterClockwise R180 = [(0, -2), (0, 1), (1, -2), (-2, 1)]
    kicks I CounterClockwise R270 = [(0, -1), (0, 2), (-2, -1), (1, 2)]
    kicks _ Clockwise R0 = [(0, -1), (1, -1), (-2, 0), (-2, -1)]
    kicks _ Clockwise R90 = [(0, -1), (-1, -1), (2, 0), (2, -1)]
    kicks _ Clockwise R180 = [(0, 1), (1, 1), (-2, 0), (-2, 1)]
    kicks _ Clockwise R270 = [(0, 1), (-1, 1), (2, 0), (2, 1)]
    kicks _ CounterClockwise R0 = [(0, 1), (1, 1), (-2, 0), (-2, 1)]
    kicks _ CounterClockwise R90 = [(0, -1), (-1, -1), (2, 0), (2, -1)]
    kicks _ CounterClockwise R180 = [(0, -1), (1, -1), (-2, 0), (-2, -1)]
    kicks _ CounterClockwise R270 = [(0, 1), (-1, 1), (2, 0), (2, 1)]

rotateMatrix :: Rotation -> Matrix a -> Matrix a
rotateMatrix Clockwise m =
  let l = (V.length m) - 1
   in flip V.imap m $
        \y r -> flip V.imap r $
          \x _ -> m @@ x @@ (l - y)
rotateMatrix CounterClockwise m =
  let l = (V.length m) - 1
   in flip V.imap m $
        \y r -> flip V.imap r $
          \x _ -> m @@ (l - x) @@ y

indexMatrix :: Integer -> Matrix (Integer, Integer)
indexMatrix s =
  V.fromList $
    flip map [0 .. s - 1] $
      \y -> V.fromList $ map ((,) y) [0 .. s - 1]

clockwiseRotationMatrix3, clockwiseRotationMatrix4 :: Matrix (Integer, Integer)
clockwiseRotationMatrix3 = rotateMatrix Clockwise $ indexMatrix 3
clockwiseRotationMatrix4 = rotateMatrix Clockwise $ indexMatrix 4

counterClockwiseRotationMatrix3, counterClockwiseRotationMatrix4 :: Matrix (Integer, Integer)
counterClockwiseRotationMatrix3 = rotateMatrix CounterClockwise $ indexMatrix 3
counterClockwiseRotationMatrix4 = rotateMatrix CounterClockwise $ indexMatrix 4

rotatePoints' :: Matrix (Integer, Integer) -> Points -> Points
rotatePoints' matrix = map $ \(y, x) -> matrix @@ fromIntegral y @@ fromIntegral x

rotatePoints :: Rotation -> Integer -> Points -> Points
rotatePoints Clockwise 3 = rotatePoints' clockwiseRotationMatrix3
rotatePoints Clockwise 4 = rotatePoints' clockwiseRotationMatrix4
rotatePoints CounterClockwise 3 = rotatePoints' counterClockwiseRotationMatrix3
rotatePoints CounterClockwise 4 = rotatePoints' counterClockwiseRotationMatrix4
rotatePoints _ _ = id

rotateTetrimino :: Rotation -> Tetrimino -> Tetrimino
rotateTetrimino r t@(Tetrimino {rotation = rv, points = p, size = s}) =
  t {rotation = rotationValue r rv, points = rotatePoints r s p}
