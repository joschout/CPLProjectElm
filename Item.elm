module Item
  ( Model, view , update
  , ItemModel (..)
  , Action (..)
  , newReminderItem, newEmailItem, itemTemplate
  , toggleTruncation, equal
  ) where

{-=
the Item module is used to represent Items.
Its Model contains the fields which all types of items have in common.
An Item Model also contains a Model from a more specific Item module,
like Reminder or Email. It kind of acts like a decorator to those modules.
=-}

import Signal
import Html exposing (..)
import Html.Attributes exposing (action, attribute, class, for, id, type', value, style)
import Html.Events exposing (on, onClick, targetValue)
import Time exposing (Time)
import String
import Email exposing (Action, update, Model, init, equal)
import Reminder exposing (Action, update, Model, init, equal)
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
    [ "Date: " ++ TimeUtil.timeToDateString TimeUtil.Dash_DMY model.date
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
      , input [ id params.id, type' params.type'
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
            } address model

-- UTILS -----------------------------------------------------------------------
toggleTruncation : Action
toggleTruncation = EmailAction (Email.ToggleTruncation)

itemTemplate : Model
itemTemplate =
  { itemModel = ReminderModel (Reminder.init "placeholder reminder body")
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

newReminderItem : String ->  String -> Bool -> Bool -> Model
newReminderItem body' date' pinned' markedAsDone' =
  { itemTemplate
    | itemModel = ReminderModel
                    <| Reminder.init body'
    , date = TimeUtil.stringToTime date'
    , pinned = pinned'
    , markedAsDone = markedAsDone'
  }

newEmailItem : String -> String -> String -> String -> String -> Model
newEmailItem fromValue toValue titleValue dateValue bodyValue =
  { itemTemplate
    | itemModel = EmailModel
                    <| Email.init fromValue toValue titleValue bodyValue
    , date = TimeUtil.stringToTime dateValue
  }

isValidDate : String -> Bool
isValidDate value =
  not (String.isEmpty value)

type alias InputFuncParams =
  { id: String -- the ID of the input
  , label: String -- the text of the label
  , type': String -- the type of input (text, email, etc.) Note that 'type' is a keyword
  , action: String -> Action -- a function that takes the value of the input and turns it into an action
  }

equal : Model -> Model -> Bool
equal item1 item2 =
  let itemModelEq =
        case (item1.itemModel, item2.itemModel) of
          (EmailModel emailModel1, EmailModel emailModel2) ->
            Email.equal emailModel1 emailModel2
          (ReminderModel reminderModel1, ReminderModel reminderModel2) ->
            Reminder.equal reminderModel1 reminderModel2
          _ ->
           False
      dateEq = item1.date == item2.date
  in itemModelEq && dateEq

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
      opacityOfItem
        = case model.markedAsDone of
            True ->
              ("opacity", "0.5")
            False ->
              ("opacity", "1")
  in style
        [ opacityOfItem
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
