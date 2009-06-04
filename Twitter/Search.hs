module Twitter.Search where
import Control.Monad
import Network.HTTP
import Network.URI
import Text.JSON
import Twitter.JSONUtils
import Twitter.Types


----- Data types -----

data SearchOptions = SearchOptions {
    resultsPerPage :: Integer,
    searchQuery :: String
  }
  deriving (Show)

data SearchResults = SearchResults [Tweet]
  deriving (Show)

----- Instance declarations -----

instance JSON SearchResults where
  showJSON _ = error "Not implemented"
  readJSON = readJSONSearchResults

instance JSON Tweet where
  showJSON _ = error "Not implemented"
  readJSON = readJSONSearchTweet


----- Data type helper functions -----
basicOptions :: String -> SearchOptions
basicOptions = SearchOptions 15

tweets :: SearchResults -> [Tweet]
tweets (SearchResults tws) = tws

----- JSON parsing -----

readJSONSearchResults :: JSValue -> Result SearchResults
readJSONSearchResults (JSObject value) = Ok $ SearchResults results
    where
    Just jsValues = lookup "results" $ fromJSObject value
    -- TODO handle missing 'results' property
    Ok results = readJSONs jsValues

readJSONSearchTweet :: JSValue -> Result Tweet
readJSONSearchTweet (JSObject value) = Ok $ Tweet text fromUser
    where
    properties = fromJSObject value
    Just (JSString jsText) = lookup "text" properties
    -- TODO handle missing 'text' property
    text = fromJSString jsText
    Just (JSString jsFromUser) = lookup "from_user" properties
    -- TODO handle missing 'from_user' property
    fromUser = fromJSString jsFromUser


----- Search functions -----

search :: SearchOptions -> IO SearchResults
search = searchJSON
-- TODO handle malformed response

searchJSON :: JSON a => SearchOptions -> IO a
searchJSON = (liftM forceDecode) . searchBody

searchBody :: SearchOptions -> IO String
searchBody options =
    do
    Right resp <- simpleHTTP $ searchRequest options
    -- TODO handle failed request
    return $ rspBody resp

searchRequest :: SearchOptions -> Request String
searchRequest options = Request {
  rqURI = searchURI options,
  rqMethod = GET,
  rqHeaders = [],
  rqBody = ""
  }

searchURI :: SearchOptions -> URI
searchURI options =
  uri
  where
    Just uri = parseURI (searchBase ++ "?" ++ params) -- this shouldn't fail
    params = urlEncodeVars $ searchParams options

searchBase = "http://search.twitter.com/search.json"

searchParams :: SearchOptions -> [(String, String)]
searchParams options = [("q", searchQuery options),
                        ("rpp", show $ resultsPerPage options)]
