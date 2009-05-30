module Main where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Twitter.Search

----- Handy command-line search client -----
main = do
  args <- getArgs
  if (null args)
    then err "Usage: twitsearch term [term ...]"
    else do
      results <- search $ unwords args
      sequence $ map putStrLn $ map tweetText $ tweets results

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure
