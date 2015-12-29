module JSONUtil
  (jsonMailbox, getJSONAndSendItToMailboxTaskSignal,  EmailKVString)
  where

import Http
import Task exposing (Task, andThen)
import Json.Decode exposing ((:=))
import Time

-- type alias to make my life easier
type alias EmailKVString = List (String, String)

-- set up mailbox
--   the signal is piped directly to main
--   the address lets us update the signal
jsonMailbox : Signal.Mailbox (List (EmailKVString))
jsonMailbox =
  Signal.mailbox []

-- the URL where our JSON lives
urlOfJSON : String
urlOfJSON = "https://api.myjson.com/bins/19lg3"

-- SIGNALS ---------------------------------------------------------------------

-- A signal that updates to the current time every second
clock : Signal Time.Time
clock =
  Time.every Time.minute

getJSONAndSendItToMailboxTaskSignal : Signal (Task Http.Error ())
getJSONAndSendItToMailboxTaskSignal =
  Signal.map (\_ -> getJSONAndSendItToMailboxTask) clock

-- TASKS -----------------------------------------------------------------------

getJSONAndSendItToMailboxTask : Task Http.Error ()
getJSONAndSendItToMailboxTask =
  safeGetJSON `andThen` sendJSON

-- TASK FOR SENDING THE EMAIL INFO TO THE MAILBOX
-- send the list of email info to our jsonMailbox
sendJSON : List (EmailKVString) -> Task x ()
sendJSON emailKVList =
  Signal.send jsonMailbox.address emailKVList

-- TASKS FOR GETTING THE EMAIL INFORMATION
-- get the list of EmailKVString values at the url
-- Note: this function may fail
getJSON : Task Http.Error (List EmailKVString)
getJSON =
  Http.get emailsJSONDecoder urlOfJSON

-- get the list of EmailKVString values at the url
-- If it should failm return an empty lis.
safeGetJSON : Task x (List EmailKVString)
safeGetJSON =
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
