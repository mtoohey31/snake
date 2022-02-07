module Random (randInRange) where

import System.Random

randInRange :: (Int, Int) -> IO Int
randInRange range = do
  g <- newStdGen
  let (r, _) = randomR range g
  return r
