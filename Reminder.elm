module Reminder
  ( Model, init, Action, update, view) where

import Html exposing (..)

-- MODEL -----------------------------------------------------------------------
type alias Model =
  { body: String
  }

init : String -> Model
init  body'=
  { body = body'
  }

-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

-- VIEW ------------------------------------------------------------------------
view : Signal.Address Action -> Model -> Html
view address model =
   p [] [ text model.body]

-- MAIN (for testing purposes) -------------------------------------------------
{-- state : Signal Model
state = Signal.foldp update (init "Dit is een testbody.") actionSignal

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

main : Signal Html
main =
   Signal.map (view actionMailbox.address) state --}
