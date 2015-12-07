module Reminder where

import Html exposing ( Html)

type alias Reminder =
  { body: String
  , created: String
  }


-- MODEL
type alias Model = Reminder

initModel : Model
initModel  =
  {body = "Dit is een testbody."
  , created = "07/12/2016"}

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

-- UPDATE
type Action = NoOp
update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

state : Signal Model
state = Signal.foldp update initModel actionSignal


view : Reminder -> Html
view model =
  Html.text <| model.body


main : Signal Html
main = Signal.map view state
