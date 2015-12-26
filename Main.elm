module Main where

import Html exposing ( Html )
import Signal

import HotKeyDecorator exposing (Model, totalActionSignal, view, update, init, Action)
import Initial exposing (initialItemList)


-- Name: Jonas Schouterden
-- Student ID: r0260385


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- The done section is visible when 2 conditions are fullfilled:
-- 1) there are items marked as done
-- 2) the visibility of the done section is on
--    (Hotkey: ALT + I)


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary: Completed
-- The visibility of the reminder section can be toggled
-- using the hotkey ALT + R


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- Start of program
{--
main : Signal Html.Html
main = Html.text "This should work."
       |> Signal.constant

main : Signal Html
main =
  Signal.map (ItemList.view actionMailbox.address) state

state : Signal ItemList.Model
state = Signal.foldp ItemList.update initialItemList ItemList.actionSignal --}
main : Signal Html
main =
  Signal.map (view actions.address) model


model : Signal Model
model = Signal.foldp update init totalActionSignal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox HotKeyDecorator.NoOp
