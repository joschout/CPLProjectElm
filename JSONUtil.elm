module JSONUtil
  (jsonMailbox)
  where

import Http
import Html exposing (Html)
import Task exposing (Task, andThen)
import Json.Decode exposing ((:=))

-- set up mailbox
--   the signal is piped directly to main
--   the address lets us update the signal
jsonMailbox : Signal.Mailbox (List (EmailKVString))
jsonMailbox =
  Signal.mailbox []

-- the URL where our JSON lives
urlOfJSON : String
urlOfJSON = "https://api.myjson.com/bins/19lg3"

-- type alias to make my life easier
type alias EmailKVString = List (String, String)

-- get the email info *and then* send the result to our mailbox
port fetchReadme : Task Http.Error ()
port fetchReadme =
  safeGetJSOn `andThen` sendJSON

-- TASK FOR SENDING THE EMAIL INFO TO THE MAILBOX ------------------------------
-- send the list of email info to our jsonMailbox
sendJSON : List (EmailKVString) -> Task x ()
sendJSON emailKVList =
  Signal.send jsonMailbox.address emailKVList

-- TASKS FOR GETTING THE EMAIL INFORMATION -------------------------------------
-- get the list of EmailKVString values at the url
-- Note: this function may fail
getJSON : Task Http.Error (List EmailKVString)
getJSON =
  Http.get emailsJSONDecoder urlOfJSON

-- get the list of EmailKVString values at the url
-- If it should failm return an empty lis.
safeGetJSOn : Task x (List EmailKVString)
safeGetJSOn =
  getJSON `Task.onError` (\err -> Task.succeed [])

-- DECODING THE JSON -----------------------------------------------------------
-- decoder for the String key-value pairs which make up the information of an email
emailFieldsJSONDecoder : Json.Decode.Decoder EmailKVString
emailFieldsJSONDecoder = Json.Decode.keyValuePairs Json.Decode.string

-- decoder for an email list
emailListJSONDecoder : Json.Decode.Decoder (List EmailKVString)
emailListJSONDecoder
  = Json.Decode.list emailFieldsJSONDecoder

-- decoder forthe list of emails as found at the URL
emailsJSONDecoder : Json.Decode.Decoder (List EmailKVString)
emailsJSONDecoder = "emails" := emailListJSONDecoder
