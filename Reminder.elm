module Reminder where

import Html exposing (..)
import Html.Events exposing (onClick)

type alias Reminder =
  { body: String
  }


-- MODEL
type alias Model = Reminder

initModel : Model
initModel  =
  { body = "Dit is een testbody."
  }

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

-- UPDATE
type Action
  = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

state : Signal Model
state = Signal.foldp update initModel actionSignal


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
   p [] [ text model.body]

main : Signal Html
main =
   Signal.map (view actionMailbox.address) state
