module HotKeyDecorator where

import AddReminderDecorator exposing (Model, focusOnPrevious, focusOnNext,
    normalSorting, reverseSorting, init, toggleTruncation, togglePinned,
    toggleDone, toggleVisibilityDoneSection, Action, checkDeadlinesOfItems,
    checkSnoozeTimeOfItems)
import Keyboard exposing (isDown, alt)
import Html exposing (..)
import Time exposing (Time)
import Debug

-- MODEL -----------------------------------------------------------------------
type alias Model = {
  addReminderDecorator : AddReminderDecorator.Model
  }

init : Model
init =
  { addReminderDecorator = AddReminderDecorator.init }
-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp
  | NextItemHK
  | PreviousItemHK
  | ToggleTruncationHK
  | TogglePinnedHK
  | ToggleDoneHK
  | ReverseSortHK
  | NormalSortHK
  | AddReminderDecoratorAction AddReminderDecorator.Action
  -- EXTENSIONS
  | ToggleVisibilityDoneSection
  | ToggleVisibilityReminderSection
  | PropagateCurrentTime Time

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    NextItemHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.focusOnNext model.addReminderDecorator) model.addReminderDecorator}
    PreviousItemHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.focusOnPrevious model.addReminderDecorator) model.addReminderDecorator }
--    ChangeSortingHK ->
--      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.changeSorting model.addReminderDecorator) model.addReminderDecorator }
    ToggleTruncationHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.toggleTruncation model.addReminderDecorator) model.addReminderDecorator }
    TogglePinnedHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.togglePinned model.addReminderDecorator) model.addReminderDecorator }
    ToggleDoneHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.toggleDone model.addReminderDecorator) model.addReminderDecorator }
    ReverseSortHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.reverseSorting model.addReminderDecorator) model.addReminderDecorator }
    NormalSortHK ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.normalSorting model.addReminderDecorator) model.addReminderDecorator }
    AddReminderDecoratorAction addReminderDecoratorAction ->
      { model | addReminderDecorator = AddReminderDecorator.update addReminderDecoratorAction model.addReminderDecorator }
    ToggleVisibilityDoneSection ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.toggleVisibilityDoneSection) model.addReminderDecorator }
    ToggleVisibilityReminderSection ->
      { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.ToggleReminderSectionVisibiliy) model.addReminderDecorator }
    PropagateCurrentTime currentTime ->
      let deadlinesCheckedModel =
            { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.checkDeadlinesOfItems currentTime) model.addReminderDecorator }
          snoozeTimeCheckedModel =
            { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.checkSnoozeTimeOfItems currentTime) deadlinesCheckedModel.addReminderDecorator }
      in { model | addReminderDecorator = AddReminderDecorator.update (AddReminderDecorator.UpdateCurrentDate currentTime) snoozeTimeCheckedModel.addReminderDecorator }
-- ACTION HOTKEY SIGNALS -------------------------------------------------------
{--
How are hot keys implemented?
GOAL: a signal of Actions
        --> contains a value when an action takes place
HOW?
  * take two signal of bool: when ALT and another key are pressed
  * make a new signal out of these signals
     ==> signal is the AND of the values of the other two
     ==> True when 2 keys are pressed, False otherwise
  * We want to do an action when the 2 keys are pressed
    ==> filter the previous signal on True values
  * make a signal with actions: map from a true signal to an action signal
--}

{--
Signal of events. The events are the given action,
and they take place when the key corresponding to the given keycode is pressed,
together with the alt-key.
INPUT:
  * an action
  * a keycode
--}
signalActionOnKeyPress : Action -> Int -> Signal Action
signalActionOnKeyPress action keyCode =
  let eventSignal = filterTrueValues --events happen when the signal is true
          <| signalBothKeysPressed alt_KeyPressed (isKeyPressed keyCode)
           -- the key corresponding to the keycode is pressed
  in Signal.map (\_-> action) eventSignal --translates an event to an action

signalActionOnKeyRelease : Action -> Int -> Signal Action
signalActionOnKeyRelease action keyCode=
--  let eventSignal = filterFalseValues
--          <| signalBothKeysPressed alt_KeyPressed
--          <| isKeyPressed keyCode
  let eventSignal = filterTrueValues
          <| signalOneKeyNotPressed alt_KeyPressed (isKeyPressed keyCode)
  in Signal.map (\_-> action) eventSignal

isKeyPressed : Int -> Signal Bool
isKeyPressed keyCode
  = Keyboard.isDown keyCode

alt_KeyPressed : Signal Bool
alt_KeyPressed = Keyboard.alt

signalBothKeysPressed : Signal Bool -> Signal Bool -> Signal Bool
signalBothKeysPressed keySignal1 keySignal2 =
  let bothKeysPressed isPressed1 isPressed2 = isPressed1 && isPressed2
  in Signal.map2 bothKeysPressed keySignal1 keySignal2

signalOneKeyNotPressed : Signal Bool -> Signal Bool -> Signal Bool
signalOneKeyNotPressed keySignal1 keySignal2 =
  let oneKeyNotPressed isPressed1 isPressed2 = (not isPressed1) || (not isPressed2)
  in Signal.map2 oneKeyNotPressed keySignal1 keySignal2

filterTrueValues : Signal Bool -> Signal Bool
filterTrueValues signal =
  let filterCondition value = if value == True
                              then True
                              else False
  in Signal.filter filterCondition False signal
--if both keys are pressed, do an action

filterFalseValues signal =
  let filterCondition value = if value == False
                              then True
                              else False
  in Signal.filter filterCondition True signal
--------------------------------------------------------------------------------

-- Alt + J : focus the next item on the feed
--    (jumps from the ‘to-do’ to the ‘done’ splits when necessary)
focusOnNextItem : Signal Action
focusOnNextItem
  = signalActionOnKeyPress NextItemHK 74

-- Alt + K : focus the previous item on the feed
--    (jumps from the ‘done’ to the ‘to-do’ splits when necessary)
focusOnPreviousItem : Signal Action
focusOnPreviousItem
 = signalActionOnKeyPress PreviousItemHK 75

-- Alt + O : toggle the truncation of the currently selected item
toggleTruncation : Signal Action
toggleTruncation
  = signalActionOnKeyPress ToggleTruncationHK 79

-- Alt + P : toggle the ‘pinned’ status of the currently selected item
togglePinned : Signal Action
togglePinned
  = signalActionOnKeyPress TogglePinnedHK 80

-- Alt + X : toggle the ‘done’ status of the currently selected item
toggleDone : Signal Action
toggleDone =
  signalActionOnKeyPress ToggleDoneHK 88

-- Alt + S : as long as this key is held down,
--    the sorting function of the feed should change to just ‘old items on top’
--    ignoring the pinned status and reversing the date priority
-- THIS IS A SPECIAL CASE
normalSorting : Signal Action
normalSorting =
  Signal.map (Debug.watch "normalSorting") (signalActionOnKeyRelease NormalSortHK 83)

reverseSorting : Signal Action
reverseSorting =
  Signal.map (Debug.watch "reverseSorting") (signalActionOnKeyPress ReverseSortHK 83)

-----------------------------
--EXTENSIONS ----------------
-----------------------------
-- ALT + I
toggleVisibilityDoneSection : Signal Action
toggleVisibilityDoneSection
  = signalActionOnKeyPress ToggleVisibilityDoneSection 73

-- Alt + R
toggleVisibilityReminderSection : Signal Action
toggleVisibilityReminderSection
 = signalActionOnKeyRelease ToggleVisibilityReminderSection 82

--------------------------------------------------------------------------------
mergedHotkeyActionSignal : Signal Action
mergedHotkeyActionSignal =
  Signal.mergeMany ([ focusOnNextItem
                    , focusOnPreviousItem
                    , toggleTruncation
                    , togglePinned
                    , toggleDone
                    , normalSorting
                    , reverseSorting
                    , toggleVisibilityDoneSection
                    , toggleVisibilityReminderSection
                    ])
-- TIME ------------------------------------------------------------------------
timedActionsSignal : Signal Action
timedActionsSignal =
  let timeAction = PropagateCurrentTime
      clockSignal = Time.every Time.second
  in Signal.map (\currentTime -> PropagateCurrentTime currentTime) clockSignal

-- VIEW ------------------------------------------------------------------------
view :  Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewAddReminderDecorator address model ]

viewAddReminderDecorator : Signal.Address Action -> Model -> Html
viewAddReminderDecorator address model =
  AddReminderDecorator.view (Signal.forwardTo address AddReminderDecoratorAction) model.addReminderDecorator

-- MAIN ------------------------------------------------------------------------
totalActionSignal : Signal Action
totalActionSignal =
  Signal.mergeMany [ mergedHotkeyActionSignal
                   , timedActionsSignal
                   ,actions.signal
                   ]


main : Signal Html
main =
  Signal.map (view actions.address) model


model : Signal Model
model = Signal.foldp update init totalActionSignal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp
