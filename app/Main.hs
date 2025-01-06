module Main (main) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Control.Time (AsMicro, CallbackKey, callbackAfter, cancelCallback, toMicro, updateCallbackToAfter)
import Data.Maybe
import Data.Time.Clock
import qualified Data.Vector as V
import Debug.Trace
import GHC.Integer (divInteger)
import Lib
import System.Random
import Tetris
import UI.NCurses

(//) = divInteger

gameHeight, gameWidth :: Integer
(gameHeight, gameWidth) = (20, 10)

type ColourMap = [(Colour, ColorID)]

data Timer = Timer
  { callbackKey :: Maybe CallbackKey,
    counter :: Integer,
    fired :: Bool,
    duration :: Integer
  }

second, halfSecond, scoreMultiplier :: Integer
second = 1000000000000
halfSecond = 500000000000
scoreMultiplier = second // 100

newTimer :: Timer
newTimer = Timer {callbackKey = Nothing, counter = 0, fired = False, duration = second}

startTimer :: MVar Timer -> Integer -> IO ()
startTimer timer duration = do
  clearTimer timer
  let duration' = picosecondsToDiffTime duration
  modifyMVar_ timer $ \timer' -> do
    k <- callbackAfter duration' $ do
      modifyMVar_ timer $ \t -> return t {counter = counter t + 1, fired = True}
      return ()
    return timer' {callbackKey = Just k, duration = duration}
  return ()

restartTimer :: MVar Timer -> IO ()
restartTimer timer =
  readMVar timer >>= \t -> do
    maybe (return ()) (flip updateCallbackToAfter $ picosecondsToDiffTime $ duration t) (callbackKey t)
    return ()

clearTimer :: MVar Timer -> IO ()
clearTimer = flip modifyMVar_ $ \t -> do
  case callbackKey t of
    Just k -> cancelCallback k
    Nothing -> return ()
  return t {callbackKey = Nothing, fired = False}

timerFired :: MVar Timer -> IO Bool
timerFired timer = readMVar timer >>= return . fired

-- timerFired timer = readMVar timer >>= return . isJust . callbackKey

timerCount :: MVar Timer -> IO Integer
timerCount timer = readMVar timer >>= return . counter

timerDuration timer = readMVar timer >>= return . duration

main :: IO ()
main = runCurses $ do
  setEcho False
  setCursorMode CursorInvisible

  w <- defaultWindow

  (height, width) <- screenSize
  let left = (width // 2) - gameWidth
  let top = (height // 2) - (gameHeight // 2)

  tetrisWindow <- newWindow (gameHeight + 2) ((gameWidth + 1) * 2) (top - 1) (left - 1)
  scoreWindow <- newWindow 1 ((gameWidth + 1) * 2) (top - 2) (left - 2)
  nextWindow <- newWindow 6 10 (top - 1) (left + (gameWidth * 2) + 2)

  randgen <- liftIO getStdGen
  let tetris = tetrinit randgen (fromIntegral gameHeight) (fromIntegral gameWidth)

  timer <- liftIO $ newMVar newTimer
  liftIO $ startTimer timer second

  colourMap <- zip colours <$> defineColours

  let context = Context w tetrisWindow scoreWindow nextWindow colourMap timer
  updateTetrisWindow context tetris
  render
  mainLoop context tetris
  where
    timerLoop :: MVar Integer -> MVar Integer -> IO (CallbackKey)
    timerLoop counter interval = do
      readMVar interval >>= \t -> callbackAfter (picosecondsToDiffTime t) $ do
        takeMVar counter >>= putMVar counter . (+ 1)
        timerLoop counter interval
        return ()

    defineColours :: Curses [ColorID]
    defineColours = do
      let orange = Color 16
      defineColor orange 1000 643 0
      let colors = [ColorCyan, ColorBlue, orange, ColorYellow, ColorGreen, ColorMagenta, ColorRed, ColorWhite]
      setColorID ColorWhite ColorBlack defaultColorID
      flip mapM (zip [1 ..] colors) $ \(i, fg) -> newColorID fg ColorBlack i

data Action = Quit | ActionDown | ActionRight | ActionLeft | ActionClockwise | ActionCounterClockwise deriving (Eq, Ord)

keyEventMap :: [(Event, Action)]
keyEventMap =
  [ (EventCharacter '`', Quit),
    (EventCharacter '\'', ActionCounterClockwise),
    (EventCharacter '.', ActionClockwise),
    (EventCharacter 'a', ActionLeft),
    (EventCharacter 'o', ActionDown),
    (EventCharacter 'e', ActionRight)
  ]

getAction :: Window -> Maybe Integer -> Curses (Maybe Action)
getAction w t = do
  ev <- getEvent w $ t
  return $ ev >>= flip lookup keyEventMap

data Context = Context
  { mainWindow :: Window,
    tetrisWindow :: Window,
    scoreWindow :: Window,
    nextWindow :: Window,
    colourMap :: ColourMap,
    timer :: MVar Timer
  }

mainLoop :: Context -> Tetris -> Curses ()
mainLoop context@(Context {timer = timer}) tetris = do
  case interrupt tetris of
    Nothing -> handleEvents
    Just GameOver -> gameOver
    Just i -> do
      liftIO $ case i of
        RestartTimer -> restartTimer timer
        StartLockTimer -> startTimer timer halfSecond
        StartGravityTimer -> startTimer timer gravitySpeed
      loop $ clearInterrupt tetris
  where
    loop = mainLoop context

    minSpeed = (second // 1000)
    gravitySpeed = max minSpeed $ second - ((score tetris) * scoreMultiplier)

    gameOver :: Curses ()
    gameOver = do
      liftIO $ clearTimer timer
      updateWindow (tetrisWindow context) $ do
        setAttribute (AttributeColor defaultColorID) True
        (h, w) <- windowSize
        let s = "   GAME OVER   "
        let y = (h // 4)
        let x = (w // 2) - ((fromIntegral $ length s) // 2)
        moveCursor y x
        drawString s
        moveCursor (y + 1) x
        drawString "Any key to quit"
      render
      getEvent (mainWindow context) Nothing
      return ()

    updateTetris :: (Tetris -> Tetris) -> Curses ()
    updateTetris f = do
      let tetris' = f tetris
      updateTetrisWindow context tetris'
      render
      loop tetris'

    handleEvents :: Curses ()
    handleEvents = do
      fired' <- liftIO $ timerFired timer
      if fired'
        then do
          onTimer
        else do
          action <- getAction (mainWindow context) (Just 100)
          case action of
            Just Quit -> quit
            Just action' -> onAction action'
            Nothing -> loop tetris

    onTimer :: Curses ()
    onTimer = do
      liftIO $ startTimer timer gravitySpeed
      updateTetris moveDown

    onAction :: Action -> Curses ()
    onAction action =
      updateTetris $
        case action of
          ActionDown -> moveDown
          ActionLeft -> moveLeft
          ActionRight -> moveRight
          ActionClockwise -> rotate Clockwise
          ActionCounterClockwise -> rotate CounterClockwise
          _ -> id

    quit :: Curses ()
    quit = do
      liftIO $ clearTimer timer
      updateWindow (tetrisWindow context) $ do
        setAttribute (AttributeColor defaultColorID) True
        (h, w) <- windowSize
        let s = "    Paused    "
        let y = (h // 4)
        let x = (w // 2) - ((fromIntegral $ length s) // 2)
        moveCursor y x
        drawString s
        moveCursor (y + 1) x
        drawString "Escape to quit"
      render
      action <- getAction (mainWindow context) Nothing
      case action of
        Just Quit -> return ()
        _ -> loop tetris

updateTetrisWindow :: Context -> Tetris -> Curses ()
updateTetrisWindow (Context {tetrisWindow = tetrisw, scoreWindow = scorew, nextWindow = nextw, colourMap = cmap}) tetris = do
  updateWindow scorew $ do
    moveCursor 0 2
    (h, w) <- windowSize
    drawLineH (Just $ Glyph ' ' []) w
    setAttribute AttributeBold True
    drawString $ "Score: "
    setAttribute AttributeBold False
    drawString $ show $ score tetris

  updateWindow nextw $ do
    flip mapM [1 .. 4] $ \y -> do
      moveCursor y 1
      drawLineH (Just $ Glyph ' ' []) 8

    let nextTetrimino' = nextTetrimino tetris
    let nextTetriminoSize = size nextTetrimino'
    setColour $ colour nextTetrimino'
    flip mapM (points nextTetrimino') $ \(y, x) -> do
      moveCursor (y + 1) (x * 2 + 1)
      drawString "██"

  updateWindow tetrisw $ do
    resetColour
    drawBox Nothing Nothing

    flip mapM [1 .. gameHeight] $ \y -> do
      moveCursor y 1
      drawLineH (Just $ Glyph ' ' []) (gameWidth * 2)

    flip V.imapM (matrix tetris) $ \y r -> do
      flip V.imapM r $ \x b -> do
        case b of
          Just (Block c) -> do
            moveCursor (fromIntegral y + 1) (fromIntegral x * 2 + 1)
            setColour c
            drawString "██"
          Nothing -> return ()

    let activeTetrimino' = activeTetrimino tetris
    setColour $ colour activeTetrimino'
    flip mapM (points activeTetrimino') $ \(y, x) -> do
      let y' = posY activeTetrimino' + y
      let x' = posX activeTetrimino' + x
      moveCursor (y' + 1) (x' * 2 + 1)
      drawString "██"
    return ()
  where
    setColour :: Colour -> Update ()
    setColour colour = do
      let cid = fromJust $ lookup colour cmap
      setAttribute (AttributeColor cid) True

    resetColour :: Update ()
    resetColour = setColour White
