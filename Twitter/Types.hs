module Twitter.Types where

data Tweet = Tweet {
    tweetText :: String,
    fromUser :: String
  }
  deriving (Show)


