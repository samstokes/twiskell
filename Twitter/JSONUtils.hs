module Twitter.JSONUtils where
import Text.JSON (JSON, decode, Result(..))

-- decodes a JSON string, throwing a runtime error if unable to decode.
forceDecode :: JSON a => String -> a
forceDecode str =
  json
  where
    Ok json = decode str

