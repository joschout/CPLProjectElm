module Main where

-- This is the main module of the project.
-- The main function in this module is used ro run the application.

import Html exposing ( Html )
import Signal

import JSONUtil exposing (jsonMailbox)
import HotKeyDecorator exposing (model, Model, view, update, actions, Action
                                , addItemsFromJSONAction, init)
import Task exposing (Task)
import Http

-- Name: Jonas Schouterden
-- Student ID: r0260385


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- The done section is visible when 2 conditions are fullfilled:
-- 1) there are items marked as done
-- 2) the visibility of the done section is on
--        (Hotkey: ALT + I)
-- Note: which item will be focused on (when using the hotkeys to focus on the next and previous item)
-- depends on the visibility of the Done section.
-- When the done section is made invisible, the done items will can not be focused on.


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- The visibility of the reminder section can be toggled
-- using the hotkey ALT + R


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- The current date is picked as default when adding reminders.
-- Note that only dates can be filled in in the input field.
-- If the input field is completely empty,
-- it will be again filled in with the current date.


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- The background of reminders of which the deadlines have past
--  is colored a light shade of red.


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- If a date earlier than the current date is picked, the item stays visible.
-- If a date later than the current date is picked, the item disappears from
-- the to do or done section.
-- EXTRA: The snoozed items can be shown using the ALT + W hotkey.
-- This will make a new section visible below the Done section.
-- In this section, the snoozed items are listed. Each item in this list also
-- shows the date until which it will be snoozed.
-- Note: which item will be focused on (when using the hotkeys to focus on the next and previous item)
-- depends on the visibility of the Snoozed section.

-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary: Unattempted


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary: Unattempted


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary: Unattempted


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- A section to see all the snoozed items can be toggled using ALT + W
-- See also the summary of the snoozed item feature.

-- get the email info *and then* send the result to our mailbox


--port fetchJSON : Task Http.Error ()
--port fetchJSON = JSONUtil.getJSONAndSendItToMailboxTask
port fetchJSON : Signal (Task Http.Error ())
port fetchJSON = JSONUtil.getJSONAndSendItToMailboxTaskSignal


-- MODEL -----------------------------------------------------------------------
type alias Model = {
  hotKeyDecorator : HotKeyDecorator.Model
}

-- ACTION ----------------------------------------------------------------------
type Action
  = NoOp
  | HotKeyDecoratorAction HotKeyDecorator.Action
  | AddItems (List(List(String, String)))

init : Model
init =
  { hotKeyDecorator = HotKeyDecorator.init}

-- UPDATE ----------------------------------------------------------------------
update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    HotKeyDecoratorAction hotKeyDecoratorAction ->
      { model
      | hotKeyDecorator = HotKeyDecorator.update hotKeyDecoratorAction model.hotKeyDecorator}

    AddItems listOfItems ->
      { model
      | hotKeyDecorator = HotKeyDecorator.update
                          (HotKeyDecorator.addItemsFromJSONAction listOfItems)
                          model.hotKeyDecorator}
-- VIEW ------------------------------------------------------------------------
view :  Signal.Address Action -> Model -> Html
view address model =
  HotKeyDecorator.view (Signal.forwardTo address HotKeyDecoratorAction) model.hotKeyDecorator


-- SIGNALS ---------------------------------------------------------------------
hotKeyActionSignal : Signal Action
hotKeyActionSignal =
  Signal.map (\hkAction -> HotKeyDecoratorAction hkAction) HotKeyDecorator.totalActionSignal

jsonResultSignal : Signal Action
jsonResultSignal =
  Signal.map AddItems jsonMailbox.signal

totalSignal : Signal Action
totalSignal =
  Signal.mergeMany [ hotKeyActionSignal
                   , jsonResultSignal
                   , actions.signal
                   ]

model : Signal Model
model = Signal.foldp update init totalSignal

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

-- Start of programs
main : Signal Html
main =
  Signal.map (view actions.address) model
