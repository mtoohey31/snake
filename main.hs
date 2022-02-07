-- TODO: make things prettier
-- cspell:ignore elem urandom pbit

import Random
import System.IO
import Term.Draw
import Term.Reserve
import Term.Size

blockChar = '█'

bitChar = '•'

blankChar = ' '

main :: IO ()
main = do
  size <- termSize
  let initialSnake = [(1, 3), (1, 2), (1, 1)]
  initialBit <- spawnBit size initialSnake
  reserveTerm
  initialDraw initialSnake initialBit
  score <- gameLoop size initialSnake initialBit
  releaseTerm
  putStrLn ("Score: " ++ show score)

gameLoop :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO Int
gameLoop size snake bit = do
  -- TODO: read directional chars concurrently and increment game loop on tick,
  -- short circuiting and exiting immediately if escape, q, etc. are ever read
  -- TODO: add test once this change has been made to make sure the recursive
  -- implementation doesn't lead to stack overflows
  ch <- getChar
  case ch of
    'q' -> return (length snake) -- quit
    ch ->
      if ch `elem` ['w', 'a', 's', 'd']
        then do
          drawAt blankChar (last snake)
          let (x, y) = head snake
          let newHead =
                ( case ch of
                    'w' -> (x, y - 1)
                    'a' -> (x - 1, y)
                    's' -> (x, y + 1)
                    'd' -> (x + 1, y)
                    _ -> error ""
                )
          if newHead == bit
            then do
              let newSnake = newHead : snake
              drawAt blockChar (head newSnake)
              newBit <- spawnBit size snake
              drawAt bitChar newBit
              gameLoop size newSnake newBit
            else
              if outOfBounds newHead size || newHead `elem` snake
                then return (length snake)
                else do
                  let newSnake = newHead : init snake
                  drawAt blockChar (head newSnake)
                  gameLoop size newSnake bit
        else gameLoop size snake bit

outOfBounds (x, y) (sizeX, sizeY) = not (x > 0 && y > 0 && x <= sizeX && y <= sizeY)

initialDraw :: [(Int, Int)] -> (Int, Int) -> IO ()
initialDraw [] pbit = drawAt bitChar pbit
initialDraw (pos : xs) pbit = do
  drawAt blockChar pos
  initialDraw xs pbit

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
