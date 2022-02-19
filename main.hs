-- TODO: make things prettier
-- TODO: support scaling argument
-- cspell:ignore elem pbit

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (when)
import Random
import System.IO
import Term

blockChar = '█'

bitChar = '•'

blankChar = ' '

exitChars = ['q', '\ESC']

inputChars = ['w', 'a', 's', 'd']

maxTickDelay = 100000

main :: IO ()
main = do
  size <- termSize
  let initialSnake = [(1, 3), (1, 2), (1, 1)]
  initialBit <- spawnBit size initialSnake
  inputKey <- newTVarIO 'd'
  score <- newTVarIO (length initialSnake)
  reserveTerm
  drawAt bitChar initialBit
  drawLine blockChar initialSnake
  drawThread <- forkIO (gameLoop size initialSnake initialBit inputKey)
  inputReader inputKey drawThread
  releaseTerm

inputReader :: TVar Char -> ThreadId -> IO ()
inputReader inputKey drawThread = do
  key <- getChar
  if key `elem` exitChars
    then killThread drawThread
    else do
      when (key `elem` inputChars) (atomically $ writeTVar inputKey key)
      inputReader inputKey drawThread

gameLoop :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> TVar Char -> IO ()
gameLoop size snake bit inputKey = do
  threadDelay (floor (maxTickDelay - (20000 * sqrt (fromIntegral (length snake)))))
  ch <- readTVarIO inputKey
  drawAt blankChar (last snake)
  let (x, y) = head snake
  let newHead =
        ( case ch of
            'w' -> (x, y - 1)
            'a' -> (x - 1, y)
            's' -> (x, y + 1)
            'd' -> (x + 1, y)
        )
  if newHead == bit
    then do
      let newSnake = newHead : snake
      drawAt blockChar (head newSnake)
      newBit <- spawnBit size (newHead : snake)
      drawAt bitChar newBit
      gameLoop size newSnake newBit inputKey
    else
      if outOfBounds newHead size || newHead `elem` snake
        then do
          drawLine blankChar (init snake)
          drawStringAt ("Game over, score: " ++ show (length snake) ++ ". Press q to exit.") (0, 0)
        else do
          let newSnake = newHead : init snake
          drawAt blockChar (head newSnake)
          gameLoop size newSnake bit inputKey

outOfBounds (x, y) (sizeX, sizeY) = not (x > 0 && y > 0 && x <= sizeX && y <= sizeY)

spawnBit :: (Int, Int) -> [(Int, Int)] -> IO (Int, Int)
spawnBit size snake = do
  let (x, y) = size
  let max = x * y
  rand <- randInRange (0, max)
  let randY = rand `div` x
  let randX = rand `mod` x
  let res = (randX + 1, randY + 1)
  if res `elem` snake
    then spawnBit size snake
    else return res
