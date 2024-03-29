module AddReminderDecorator
  ( Model, view, update, Action(..)
  , focusOnPrevious, focusOnNext
  , normalSorting, reverseSorting
  , togglePinned, toggleDone, toggleTruncation
  , checkDeadlinesOfItems, checkSnoozeTimeOfItems
  , toggleVisibilityDoneSection, toggleVisibilitySnoozedSection
  , addItemsFromJSONAction, init
  ) where
{--
The AddReminderDecorator module contains a Model and functions for adding
reminders to the item feed. Its Model kind of acts like a decorator around
the Model from the itemList module.

 This module is partially based on
"Real World Elm - Part 2 - Form Validation"
http://engineering.truqu.com/2015/09/25/real-world-elm-part-2.html
--}

import Html exposing (..)
import Html.Attributes exposing (action, attribute, class, for, id, type', value, style)
import Html.Events exposing (on, onClick, targetValue)
import Dict exposing (Dict)
import String
import ItemList
import Time exposing (Time)
import TimeUtil exposing (DateFormat, timeToDateString)

-- MODEL -----------------------------------------------------------------------
-- type to represent the state of the input elements of the reminder
type InputState
  = Initial
  | HasError String
  | IsOkay

{--
Our Model is a record with three fields
--}
type alias Model =
  { itemList: ItemList.Model
  , reminderBody : String -- the value of the reminderBody input
  , dateInputValue : String -- the value of the date input
  , inputState : Dict String InputState
    -- a dictionary that maps the IDs of the inputs to their state
    -- String is the Key which maps to a value of InputState
    --EXTENSIONS:
  , reminderSectionVisible : Bool
  , currentDate : Time
  }

init : Model
init =
  { itemList = ItemList.initialStaticItemList
  , reminderBody = ""
  , dateInputValue = ""
  , inputState = Dict.empty
  , reminderSectionVisible = True
  , currentDate = 0
  }

-- UPDATE ----------------------------------------------------------------------

type Action
  = NoOp
  | SetReminderBody String
  | SetReminderDate String
  | SetInputState
  | SubmitReminder
  | ItemListAction ItemList.Action
  | SetItemList ItemList.Model
  -- EXTENSIONS
  | ToggleReminderSectionVisibiliy
  | UpdateCurrentDate Time

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SetReminderBody reminderBody' ->
      { model | reminderBody = reminderBody' }

    SetReminderDate date' ->
      { model | dateInputValue = date' }

    SetInputState ->
      let reminderBody = if isValidReminderBody model.reminderBody
                 then IsOkay
                 else HasError "Please enter a reminder message"
          date = if isValidDate model.dateInputValue
                then IsOkay
                else HasError "Please enter a valid date"
          inputState' = Dict.fromList [ ("reminderBody", reminderBody), ("date", date)]
      in
        { model | inputState = inputState' }

    SubmitReminder ->
      submitReminder model

    ItemListAction itemListAction ->
      { model | itemList = ItemList.update itemListAction model.itemList}

    SetItemList itemListModel ->
      { model | itemList = itemListModel }

    ToggleReminderSectionVisibiliy ->
      { model | reminderSectionVisible = (not model.reminderSectionVisible) }

    UpdateCurrentDate currentTime ->
      { model | currentDate = currentTime
              , dateInputValue =
                  if String.isEmpty model.dateInputValue
                    then TimeUtil.timeToDateString TimeUtil.Dash_YMD currentTime
                    else model.dateInputValue}

submitReminder : Model -> Model
submitReminder model =
  let itemListAction = ItemList.AddReminder model.reminderBody model.dateInputValue False False
  in { model | itemList = ItemList.update itemListAction model.itemList}

-- VIEW -----------------------------------------------------------------------
view :  Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewItemList address model
    , viewReminder address model
    ]

viewItemList : Signal.Address Action -> Model -> Html
viewItemList address model =
  ItemList.view (Signal.forwardTo address ItemListAction) model.itemList

viewReminder : Signal.Address Action -> Model -> Html
viewReminder address model =
  case model.reminderSectionVisible of
    True ->
      div [ class "container" , reminderSectionStyle]
      [ reminderHeader
      , div [ attribute "role" "form" ]
        [ reminderBodyInput address model
        , dateInput address model
        , button [ class "btn btn-default"
                  , onClick address
                      <| if isValid model then SubmitReminder
                                          else SetInputState
                  ]
          [ text "Add" ]
        ]
      ]

    False ->
      div [] []

reminderHeader : Html
reminderHeader =
  h1 [] [text "Reminder"]

type alias InputFuncParams =
  { id: String -- the ID of the input
  , label: String -- the text of the label
  , type': String -- the type of input (text, email, etc.) Note that 'type' is a keyword
  , action: String -> Action -- a function that takes the value of the input and turns it into an action
  , value: String -- the value that must be shown in the input field
  }

inputFunc : InputFuncParams -> Signal.Address Action -> Model -> Html
inputFunc params address model =
  div [ class "form-group" ]
      [ label [ for params.id ]
              [ text params.label ]
      , input [ id params.id, type' params.type' , value params.value
              , class "form-control"
              , on "input" targetValue
                  (Signal.message address << params.action)
              ]
              []
      ]
reminderBodyInput : Signal.Address Action -> Model -> Html
reminderBodyInput address model =
  inputFunc { id = "reminderBody"
            , label = "Reminder message: "
            , type' = "text"
            , action = SetReminderBody
            , value = model.reminderBody
            } address model

dateInput : Signal.Address Action -> Model -> Html
dateInput address model =
  inputFunc { id = "date"
            , label = "Date: "
            , type' = "date"
            , action = SetReminderDate
            , value =
               if String.isEmpty model.dateInputValue
                then TimeUtil.timeToDateString TimeUtil.Dash_YMD model.currentDate
                else model.dateInputValue
            } address model

-- VALIDATION OF INPUT STRINGS ------------------------------------------------

isValidReminderBody : String -> Bool
isValidReminderBody =
  not << String.isEmpty

isValidDate : String -> Bool
isValidDate value =
  not (String.isEmpty value)

isValid : Model -> Bool
isValid model =
  isValidReminderBody model.reminderBody && isValidDate model.dateInputValue

-- EXTRA INTERFACE TO EXTERN ---------------------------------------------------

focusOnNext : Model -> Action
focusOnNext model =
  ItemListAction (ItemList.focusOnNextItemAction model.itemList)


focusOnPrevious : Model -> Action
focusOnPrevious model =
  ItemListAction (ItemList.focusOnPreviousItemAction model.itemList)

normalSorting : Model -> Action
normalSorting model =
  ItemListAction (ItemList.normalSortingAction model.itemList)

reverseSorting : Model -> Action
reverseSorting model =
  ItemListAction (ItemList.reverseSortingAction model.itemList)

toggleTruncation : Model -> Action
toggleTruncation model =
  ItemListAction (ItemList.toggleTruncationAction model.itemList)

togglePinned : Model -> Action
togglePinned model =
  ItemListAction (ItemList.togglePinnedAction model.itemList)

toggleDone : Model -> Action
toggleDone model =
  ItemListAction (ItemList.toggleDoneAction model.itemList)

toggleVisibilityDoneSection : Action
toggleVisibilityDoneSection  =
  ItemListAction (ItemList.ToggleVisiblityDoneItems)

checkDeadlinesOfItems : Time.Time -> Action
checkDeadlinesOfItems currentTime =
  ItemListAction (ItemList.CheckDeadlines currentTime)

checkSnoozeTimeOfItems : Time.Time -> Action
checkSnoozeTimeOfItems currentTime =
  ItemListAction (ItemList.CheckSnoozeTimes currentTime)

toggleVisibilitySnoozedSection : Action
toggleVisibilitySnoozedSection =
  ItemListAction (ItemList.ToggleVisibilitySnoozedSection)

addItemsFromJSONAction : List(List(String, String)) ->  Action
addItemsFromJSONAction listOfItems =
  ItemListAction (ItemList.AddItemsFromJSON listOfItems)
-- STYLE -----------------------------------------------------------------------

reminderSectionStyle : Attribute
reminderSectionStyle =
  style
    [ ("width", "40%")
    , ("margin", "auto")
    ]

-- SIGNALS ---------------------------------------------------------------------

main : Signal Html
main =
  Signal.map (view actions.address) model


model : Signal Model
model = Signal.foldp update init actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp
