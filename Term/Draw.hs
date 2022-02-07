module Term.Draw (drawAt) where

drawAt :: Char -> (Int, Int) -> IO ()
drawAt c (x, y) = do
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H" ++ [c])
