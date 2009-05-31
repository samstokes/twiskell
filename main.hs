module Main where
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Twitter.Search

----- Helpers -----
formatTweet :: Tweet -> String
formatTweet tweet = '@' : fromUser tweet ++ ": " ++ tweetText tweet

----- Handy command-line search client -----
usage = "Usage: twitsearch <OPTIONS> term [term ...]"

main = do
  args <- getArgs
  options <- parseArgs args
  results <- search options
  sequence $ map putStrLn $ map formatTweet $ tweets results

options :: [OptDescr Integer]
options = [Option ['n'] ["max"] (ReqArg read "NUM") "max results (default 15)"]

parseArgs :: [String] -> IO SearchOptions
parseArgs argv =
  case getOpt' Permute options argv of
    ([count], term:terms, [], []) -> return SearchOptions {
      resultsPerPage = count,
      searchQuery = unwords (term:terms)
      }
    ([], term:terms, [], []) -> return $ basicOptions $ unwords (term:terms)
    (_, _, unrecog:unrecogs, _) -> do
      sequence $ map unrecognised (unrecog:unrecogs)
      err $ usageInfo usage options
    (_, _, _, errr:errrs) -> do
      sequence $ map putStrLn (errr:errrs)
      err $ usageInfo usage options
    _ -> err $ usageInfo usage options

unrecognised :: String -> IO ()
unrecognised opt = putStrLn ("Unrecognised option " ++ opt)

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure
