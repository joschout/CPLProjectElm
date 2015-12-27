module Item where

import Signal
import Html exposing (..)
import Html.Attributes exposing (action, attribute, class, for, id, type', value, style)
import Html.Events exposing (on, onClick, targetValue)
import Time exposing (Time)
import String
import Email exposing (Action, update, Model, initModel)
import Reminder exposing (Action, update, Model, init)
import TimeUtil exposing (timeToDateString, stringToTime, DateFormat)
import Dict exposing (Dict)

-- MODEL -----------------------------------------------------------------------
type ItemModel
  = EmailModel Email.Model
  | ReminderModel Reminder.Model

type alias Model =
  { itemModel : ItemModel
  , date : Time -- Time is comparable
  , pinned : Bool
  , markedAsDone : Bool
  , isFocused : Bool
  -- EXTENSIONS
  , isPastDeadline : Bool
  --Snooze
  , isSnoozed : Bool
  , snoozeDateInputValue : String
  , snoozedUntilDate : Time
  , snoozeInputState : Dict String SnoozeInputState
  }

-- type to represent the state of the input elements of the snooze
type SnoozeInputState
  = Initial
  | HasError String
  | IsOkay

-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp
  | ToggleMarkedAsDone
  | TogglePinned
  | EmailAction Email.Action
  | ReminderAction Reminder.Action
  | Focus Bool
  -- EXTENSIONS
  | CheckDeadline Time
  --
  | Snooze
  | SetSnoozeDate String
  | SetSnoozeInputState
  | CheckSnoozeTime Time

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    ToggleMarkedAsDone ->
       { model | markedAsDone = not (model.markedAsDone) }
    TogglePinned ->
      { model | pinned = (not model.pinned) }
    EmailAction emailAction ->
      updateEmailAction emailAction model
    ReminderAction reminderAction ->
      updateReminderAction reminderAction model
    Focus focusBool ->
      { model | isFocused = focusBool }
    CheckDeadline currentTime ->
        let deadline = model.date
        in { model | isPastDeadline = (deadline < currentTime) }
    Snooze ->
      { model |  snoozedUntilDate = TimeUtil.stringToTime model.snoozeDateInputValue }
    SetSnoozeDate date' ->
      { model | snoozeDateInputValue = date' }
    SetSnoozeInputState ->
      let newSnoozeDateInputValue = if isValidDate model.snoozeDateInputValue
                then IsOkay
                else HasError "Please enter a valid date"
          snoozeInputState'
              = Dict.fromList [("snoozeDateInputValue", newSnoozeDateInputValue)]
      in { model | snoozeInputState = snoozeInputState' }
    CheckSnoozeTime currentTime ->
      let snoozeTime = model.snoozedUntilDate
      in { model | isSnoozed = (currentTime < snoozeTime) }

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


-- VIEW ------------------------------------------------------------------------
view : Signal.Address Action -> Model -> Html
view address model =
  div [selectedItemStyle model.isFocused]
    [ div [ itemStyle model ]
      [ viewItem address model.itemModel
      , viewMarkAsDoneButton address model
      , viewPinButton address model
      , viewDate model
      , viewSnoozeSection address model
      ]
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
  [ onClick address ToggleMarkedAsDone ]  --onClick address (MarkAsDone (not model.MarkAsDone)) ]
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
  [ onClick address TogglePinned ]  --onClick address (MarkAsDone (not model.MarkAsDone)) ]
  [ text <| pinButtonText model ]

pinButtonText : Model -> String
pinButtonText model =
  case model.pinned of
    True
      -> "Unpin"
    False
      -> "Pin"
-- Date
viewDate : Model -> Html
viewDate model =
  p
    []
    [ "date: " ++ TimeUtil.timeToDateString TimeUtil.Dash_DMY model.date
      |> text ]

-- Snooze section
viewSnoozeSection : Signal.Address Action -> Model -> Html
viewSnoozeSection address model =
  div [ class "container" ]
  [ showSnoozeDate model
  , div [ attribute "role" "form" ]
    [ dateInput address model
    , button [ class "btn btn-default"
              , onClick address
                  <| if isValidDate model.snoozeDateInputValue then Snooze
                     else SetSnoozeInputState
              ]
      [ text "Snooze" ]
    ]
  ]

showSnoozeDate : Model -> Html
showSnoozeDate model =
  case model.isSnoozed of
    True ->
      div
        [style [("padding", "5px 0px 5px")] ]
        [ text
                <| "This item is snoozed until: "
                  ++ (TimeUtil.timeToDateString TimeUtil.Dash_DMY model.snoozedUntilDate)
        ]
    False -> text ""

inputFunc : InputFuncParams -> Signal.Address Action -> Model -> Html
inputFunc params address model =
  div [ class "form-group" ]
      [ label [ for params.id ]
              [ text params.label ]
      , input [ id params.id, type' params.type' --, value params.value
              , class "form-control"
              , on "input" targetValue
                  (Signal.message address << params.action)
              ]
              []
      ]

dateInput : Signal.Address Action -> Model -> Html
dateInput address model =
  inputFunc { id = "date"
            , label = "Snooze until: "
            , type' = "date"
            , action = SetSnoozeDate
--            , value = if String.isEmpty model.snoozeDateInputValue
--                      then TimeUtil.timeToDateString TimeUtil.Slash_YMD model.currentDate
--                      else model.snoozeDateInputValue
            } address model


-- MAIN, STATE & SIGNALS -------------------------------------------------------
actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

state : Signal Model
state = Signal.foldp update initModel actionSignal

main =
   Signal.map (view actionMailbox.address) state

initModel : Model
initModel =
  { itemModel = EmailModel Email.initModel
    --itemModel = ReminderModel Reminder.initModel
  , date = TimeUtil.stringToTime "2015-01-30"
  , pinned = False
  , markedAsDone = False
  , isFocused = False
  , isPastDeadline = False
  , isSnoozed = False
  , snoozeDateInputValue = ""
  , snoozedUntilDate = 0
  , snoozeInputState = Dict.empty
  }
-- UTILS -----------------------------------------------------------------------

toggleTruncation : Action
toggleTruncation = EmailAction (Email.ToggleTruncation)


newReminderItem : String ->  String -> Bool -> Bool -> Model
newReminderItem body' date' pinned' markedAsDone' =
  { itemModel = ReminderModel
                  <| Reminder.init body'
  , date = TimeUtil.stringToTime date'
  , pinned = pinned'
  , markedAsDone = markedAsDone'
  , isFocused = False
  , isPastDeadline = False
  , isSnoozed = False
  , snoozeDateInputValue = ""
  , snoozedUntilDate = 0
  , snoozeInputState = Dict.empty
  }


isValidDate : String -> Bool
isValidDate value =
  not (String.isEmpty value)

type alias InputFuncParams =
  { id: String -- the ID of the input
  , label: String -- the text of the label
  , type': String -- the type of input (text, email, etc.) Note that 'type' is a keyword
  , action: String -> Action -- a function that takes the value of the input and turns it into an action
--  , value: String -- the value that must be shown in the input field
  }

-- STYLE -----------------------------------------------------------------------
selectedItemStyle : Bool -> Attribute
selectedItemStyle isFocused =
  case isFocused of
    True ->
      style
        [ ("border-left-width", "thick")
        , ("border-left-style", "double")
        , ("border-left-color", "rgb(170, 255, 255)")
        ]
    False -> style []

itemStyle : Model -> Attribute
itemStyle model =
  let backGroundColor
        = case shouldBeMarkedAsPastDeadline model of
            True ->
                ("background-color", "rgb(227, 166, 170)")
            False ->
                ("background-color", "rgb(255, 255, 255)")
  in style
        [ ("opacity", "1")
        , ("padding", "10px 10px 20px")
        , ("border-bottom-width", "thick")
        , ("border-bottom-style", "solid")
        , ("border-bottom-color", "rgb(150, 150, 150)")
        , backGroundColor
        ]
shouldBeMarkedAsPastDeadline : Model -> Bool
shouldBeMarkedAsPastDeadline model
 = case model.itemModel of
   ReminderModel _ ->
     model.isPastDeadline
   EmailModel _ -> False

snoozeSectionStyle : Attribute
snoozeSectionStyle =
  style
    [ ("width", "40%")
    , ("margin", "auto")
    ]
