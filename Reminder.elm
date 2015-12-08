module Reminder where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type alias Reminder =
  { body: String
  , created: String
  , pinned: Bool
  , markedAsDone: Bool
  }


-- MODEL
type alias Model = Reminder

initModel : Model
initModel  =
  { body = "Dit is een testbody."
  , created = "07/12/2016"
  , pinned = False
  , markedAsDone = False }

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

-- UPDATE
type Action = NoOp | MarkAsDone Bool | Pin Bool


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    MarkAsDone markBool ->
       {model | markedAsDone = markBool}
    Pin pinBool ->
      {model | pinned = pinBool}

state : Signal Model
state = Signal.foldp update initModel actionSignal

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [ reminderStyle ] [ text model.body]
    , viewMarkAsDoneButton address model
    , viewPinButton address model]

-- MarkAsDone button
viewMarkAsDoneButton : Signal.Address Action -> Model -> Html
viewMarkAsDoneButton address model =
  button
  [ not model.markedAsDone
      |> MarkAsDone
      |> onClick address]  --onClick address (MarkAsDone (not model.MarkAsDone)) ]
  [ text <| markAsDoneButtonText model ]

markAsDoneButtonText : Model -> String
markAsDoneButtonText model =
  case model.markedAsDone of
    True
      -> "Undo"
    False
      -> "Mark as Done"

-- Pin button
viewPinButton : Signal.Address Action -> Model -> Html
viewPinButton address model =
  button
  [ not model.pinned
      |> Pin
      |> onClick address]  --onClick address (MarkAsDone (not model.MarkAsDone)) ]
  [ text <| pinButtonText model ]

pinButtonText : Model -> String
pinButtonText model =
  case model.pinned of
    True
      -> "Unpin"
    False
      -> "Pin"

reminderStyle : Attribute
reminderStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "100%")
    , ("text-align", "left")
    ]
main : Signal Html
main =
   Signal.map (view actionMailbox.address) state
