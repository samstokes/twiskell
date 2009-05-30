module Twitter.Search where
import Control.Monad
import Network.HTTP
import Network.URI
import Text.JSON


----- Data types -----

data SearchResults = SearchResults [SearchResult]
  deriving (Show)

data SearchResult = SearchResult String
  deriving (Show)


----- Instance declarations -----

instance JSON SearchResults where
  showJSON _ = error "Not implemented"
  readJSON = readJSONSearchResults

instance JSON SearchResult where
  showJSON _ = error "Not implemented"
  readJSON = readJSONSearchResult


----- JSON parsing -----

readJSONSearchResults :: JSValue -> Result SearchResults
readJSONSearchResults (JSObject value) = Ok $ SearchResults results
    where
    Just jsValues = lookup "results" $ fromJSObject value
    -- TODO handle missing 'results' property
    Ok results = readJSONs jsValues

readJSONSearchResult :: JSValue -> Result SearchResult
readJSONSearchResult (JSObject value) = Ok $ SearchResult text
    where
    Just (JSString jsText) = lookup "text" $ fromJSObject value
    -- TODO handle missing 'text' property
    text = fromJSString jsText


----- Search functions -----

search :: String -> IO SearchResults
search = searchJSON
-- TODO handle malformed response

searchJSON :: JSON a => String -> IO a
searchJSON = (liftM forceDecode) . searchBody

-- decodes a JSON string, throwing a runtime error if unable to decode.
forceDecode :: JSON a => String -> a
forceDecode str =
  json
  where
    Ok json = decode str

searchBody :: String -> IO String
searchBody searchTerms =
    do
    Right resp <- simpleHTTP $ searchRequest searchTerms
    -- TODO handle failed request
    return $ rspBody resp

searchRequest :: String -> Request String
searchRequest searchTerms = Request {
  rqURI = searchURI searchTerms,
  rqMethod = GET,
  rqHeaders = [],
  rqBody = ""
  }

searchURI :: String -> URI
searchURI searchTerms =
  uri
  where
    Just uri = parseURI (searchBase ++ "?" ++ params) -- this shouldn't fail
    params = urlEncodeVars [("q", searchTerms)]

searchBase = "http://search.twitter.com/search.json"
