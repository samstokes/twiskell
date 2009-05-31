module Main where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Twitter.Search

----- Helpers -----
formatTweet :: Tweet -> String
formatTweet tweet = '@' : fromUser tweet ++ ": " ++ tweetText tweet

----- Handy command-line search client -----
main = do
  args <- getArgs
  if (null args)
    then err "Usage: twitsearch term [term ...]"
    else do
      results <- search $ basicOptions $ unwords args
      sequence $ map putStrLn $ map formatTweet $ tweets results

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure
