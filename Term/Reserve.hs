module Term.Reserve (reserveTerm, releaseTerm) where

import System.IO (BufferMode (LineBuffering, NoBuffering), hFlush, hSetBuffering, hSetEcho, hSetNewlineMode, noNewlineTranslation, stdin, stdout)

reserveTerm = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putStr "\ESC[?1049h" -- enable alternate buffer
  putStr "\ESC[?25l" -- hide cursor

releaseTerm = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetEcho stdin True
  putStr "\ESC[2J" -- clear screen
  putStr "\ESC[?1049l" -- disable alternate buffer
  putStr "\ESC[?25h" -- show cursor
