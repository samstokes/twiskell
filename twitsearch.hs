module Twitter.Search where
import Network.HTTP
import Network.URI
import Text.JSON

data SearchResults = SearchResults [SearchResult]
  deriving (Show)

data SearchResult = SearchResult String
  deriving (Show)

instance JSON SearchResults where
  showJSON _ = error "Not implemented"
  readJSON = readJSONSearchResults

instance JSON SearchResult where
  showJSON _ = error "Not implemented"
  readJSON = readJSONSearchResult

readJSONSearchResults :: JSValue -> Result SearchResults
readJSONSearchResults (JSObject value) = Ok $ SearchResults results
    where
    Just jsValues = lookup "results" $ fromJSObject value
    Ok results = readJSONs jsValues

readJSONSearchResult :: JSValue -> Result SearchResult
readJSONSearchResult (JSObject value) = Ok $ SearchResult text
    where
    Just (JSString jsText) = lookup "text" $ fromJSObject value
    text = fromJSString jsText

twitterSearch :: String -> IO SearchResults
twitterSearch query =
    do
    (Right resp) <- simpleHTTP request
    let body = rspBody resp
    let (Ok results) = decode body
    return results
    where
    request = Request uri GET [] ""
    (Just uri) = parseURI searchURL
    searchURL = "http://search.twitter.com/search.json?" ++ searchParams
    searchParams = urlEncodeVars [("q", query)]

twitterSearchBody :: String -> IO String
twitterSearchBody query =
    do
    (Right resp) <- simpleHTTP request
    return $ rspBody resp
    where
    request = Request uri GET [] ""
    (Just uri) = parseURI searchURL
    searchURL = "http://search.twitter.com/search.json?" ++ searchParams
    searchParams = urlEncodeVars [("q", query)]
