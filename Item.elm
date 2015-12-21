module Item where

import Signal
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Time)

import Email exposing (Action, update, Model, initModel)
import Reminder exposing (Action, update, Model, initModel)
import TimeUtil exposing (timeToString, stringToTime)

-- MODEL
actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

type ItemModel
  = EmailModel Email.Model
  | ReminderModel Reminder.Model

type alias Model =
  { itemModel : ItemModel
  , date : Time -- Time is comparable
  , pinned : Bool
  , markedAsDone : Bool
  }

-- UPDATE
type Action
  = NoOp
  | MarkAsDone Bool
  | Pin Bool
  | EmailAction Email.Action
  | ReminderAction Reminder.Action

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    MarkAsDone markBool ->
       { model | markedAsDone = markBool }
    Pin pinBool ->
      { model | pinned = pinBool }
    EmailAction emailAction ->
      updateEmailAction emailAction model
    ReminderAction reminderAction ->
      updateReminderAction reminderAction model

updateEmailAction : Email.Action -> Model -> Model
updateEmailAction emailAction model =
  case model.itemModel of
    EmailModel emailModel ->
      { model | itemModel
          = Email.update emailAction emailModel
          |> EmailModel }
    ReminderModel reminderModel ->
      model
updateReminderAction : Reminder.Action -> Model -> Model
updateReminderAction reminderAction model =
  case model.itemModel of
    EmailModel emailModel ->
      model
    ReminderModel reminderModel ->
      { model | itemModel
          =  Reminder.update reminderAction reminderModel
          |> ReminderModel }

state : Signal Model
state = Signal.foldp update initModel actionSignal

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div [ itemStyle ]
    [ viewItem address model.itemModel
    , viewMarkAsDoneButton address model
    , viewPinButton address model
    , viewDate model
    ]

-- viewItem
viewItem : Signal.Address Action -> ItemModel -> Html
viewItem address itemModel =
  case itemModel of
    EmailModel emailModel  ->
      Email.view  (Signal.forwardTo address EmailAction) emailModel
    ReminderModel reminderModel ->
      Reminder.view  (Signal.forwardTo address ReminderAction) reminderModel

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
viewDate : Model -> Html
viewDate model =
  p
    []
    [ "date: " ++ TimeUtil.timeToString model.date
      |> text ]

-- MAIN
main : Signal Html
main =
   Signal.map (view actionMailbox.address) state

initModel : Model
initModel =
  { itemModel = EmailModel Email.initModel
    --itemModel = ReminderModel Reminder.initModel
  , date = TimeUtil.stringToTime "2015-01-30"
  , pinned = False
  , markedAsDone = False
  }

itemStyle : Attribute
itemStyle =
  style
    [ ("opacity", "1")
    , ("padding", "10px 10px 20px")
    , ("border-bottom-width", "thick")
    , ("border-bottom-style", "solid")
    , ("border-bottom-color", "rgb(250, 250, 250)")
    , ("background-color", "rgb(255, 255, 255)")
    ]
