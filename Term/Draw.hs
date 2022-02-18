module Term.Draw (drawAt, drawLine, drawStringAt) where

drawAt :: Char -> (Int, Int) -> IO ()
drawAt char (x, y) = do
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H" ++ [char])

drawLine :: Char -> [(Int, Int)] -> IO ()
drawLine _ [] = return ()
drawLine char (pos : xs) = do
  drawAt char pos
  drawLine char xs

drawStringAt :: String -> (Int, Int) -> IO ()
drawStringAt string (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H" ++ string)
