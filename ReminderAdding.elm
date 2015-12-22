module ReminderAdding where
{-- This module is based on
"Real World Elm - Part 2 - Form Validation"
http://engineering.truqu.com/2015/09/25/real-world-elm-part-2.html
--}

import Html exposing (..)
import Html.Attributes exposing (action, attribute, class, for, id, type')
import Html.Events exposing (on, onClick, targetValue)
import Dict exposing (Dict)
import String

-- MODEL
-- type to represent the state of the input elements
type InputState
  = Initial
  | HasError String
  | IsOkay

{--
Our Model is a record with three fields
--}
type alias Model =
  { reminderBody : String -- the value of the reminderBody input
  , date : String -- the value of the date input
  , inputState : Dict String InputState
    -- a dictionary that maps the IDs of the inputs to their state
    -- String is the Key which maps to a value of InputState
  }

init : Model
init =
  { reminderBody = ""
  , date = ""
  , inputState = Dict.empty
  }

-- UPDATE

type Action
  = NoOp
  | SetReminderBody String
  | SetDate String
  | SetInputState
  | SubmitReminder


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SetReminderBody reminderBody' ->
      { model | reminderBody = reminderBody' }
    SetDate date' ->
      { model | date = date' }
    SetInputState ->
      let reminderBody = if isValidReminderBody model.reminderBody
                 then IsOkay
                 else HasError "Please enter a reminder message"
          date = if isValidDate model.date
                then IsOkay
                else HasError "Please enter a valid date"
          inputState' = Dict.fromList [ ("reminderBody", reminderBody), ("date", date)]
      in
        { model | inputState = inputState' }
    SubmitReminder ->
      model

isValidReminderBody : String -> Bool
isValidReminderBody =
  not << String.isEmpty

isValidDate : String -> Bool
isValidDate value =
  case String.toInt value of
    Ok int ->
      int >= 0
    Err _ ->
      False

isValid : Model -> Bool
isValid model =
  isValidReminderBody model.reminderBody && isValidDate model.date



-- SIGNALS

main : Signal Html
main =
  Signal.map view model


model : Signal Model
model = Signal.foldp update init actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


-- VIEW
{--
<div>
  <div class="container">
    <div role="form">
      <div class="form-group">
        <label for="reminderBody">reminderBody</label>
        <input id="reminderBody" type="text" class="form-control">
      </div>
      <div class="form-group">
        <label for="date">date</label>
        <input id="date" type="text" class="form-control">
      </div>
      <button class="btn btn-default">SubmitReminder</button>
    </div> --ends div container
  </div> --ends div form
</div> --ends div container
--}
type alias InputFuncParams =
  { id: String -- the ID of the input
  , label: String -- the text of the label
  , type': String -- the type of input (text, email, etc.) Note that 'type' is a keyword
  , action: String -> Action -- a function that takes the value of the input and turns it into an action
  }

inputFunc : InputFuncParams -> Model -> Html
inputFunc params model =
  div [ class "form-group" ]
      [ label [ for params.id ]
              [ text params.label ]
      , input [ id params.id, type' params.type' , class "form-control"
              , on "input" targetValue
                  (Signal.message actions.address << params.action)
              ]
              []
      ]
reminderBodyInput : Model -> Html
reminderBodyInput =
  inputFunc { id = "reminderBody"
            , label = "Reminder message"
            , type' = "text"
            , action = SetReminderBody
            }
dateInput : Model -> Html
dateInput =
  inputFunc { id = "date"
            , label = "date"
            , type' = "text"
            , action = SetDate
            }

view : Model -> Html
view model =
  div [ class "container" ]
  [ div [ attribute "role" "form" ]
    [ reminderBodyInput model
    , dateInput model
    , button [ class "btn btn-default"
              , onClick actions.address
                  <| if isValid model then SubmitReminder
                                      else SetInputState
              ]
      [ text "Add" ]
    ]
  ]
