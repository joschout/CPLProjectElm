module Email where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String

type alias Email =
  { from: String
  , to: String
  , title: String
  , body: String
  , date: String
  , pinned: Bool
  , markedAsDone: Bool
  , isTruncated : Bool
  }

-- MODEL
type alias Model = Email

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

-- UPDATE
type Action = NoOp | MarkAsDone Bool | Pin Bool | Truncate Bool

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    MarkAsDone markBool ->
       { model | markedAsDone = markBool }
    Pin pinBool ->
      { model | pinned = pinBool }
    Truncate truncateBool ->
      { model | isTruncated = truncateBool }


state : Signal Model
state = Signal.foldp update initModel actionSignal

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewEmailHeading model
    , viewEmailBody model
    , viewMoreButton address model
    , viewMarkAsDoneButton address model
    , viewPinButton address model]

viewEmailHeading : Model -> Html
viewEmailHeading model =
  div
    [ emailStyle ]
    [ model.title ++ " | " ++ model.from ++ " says:"
      |> text ]

viewEmailBody : Model -> Html
viewEmailBody model =
  let canBeTruncated = allowedToBeTruncated model.body
  in case canBeTruncated && model.isTruncated of
    True ->
      div
        []
        [ String.left 200 model.body
          |> text ]
    False ->
      div [] [ text model.body]

-- More Button
viewMoreButton : Signal.Address Action -> Model -> Html
viewMoreButton address model =
  let canBeTruncated = allowedToBeTruncated model.body
  in case canBeTruncated of
    True ->
      button
      [ not model.isTruncated
          |> Truncate
          |> onClick address]  --onClick address (MarkAsDone (not model.MarkAsDone)) ]
      [ text <| moreButtonText model ]
    False ->
      div [] []

allowedToBeTruncated : String -> Bool
allowedToBeTruncated string =
  let stringLength = String.length string
  in if stringLength > 200  then True
    else False

moreButtonText : Model -> String
moreButtonText model =
  case model.isTruncated of
    True
      -> "More"
    False
      -> "Less"


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

-- MAIN
main : Signal Html
main =
   Signal.map (view actionMailbox.address) state


emailStyle : Attribute
emailStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "100%")
    , ("text-align", "left")
    ]




initModel : Model
initModel  =
  { from = "bossman@corporate.me"
  , to = "manager@corporate.me"
  , title = "Corporate Ipsum"
  , body = """Collaboratively administrate empowered markets via plug-and-play
              networks. Dynamically procrastinate B2C users after installed base
              benefits. Dramatically visualize customer directed convergence without
              revolutionary ROI.

              Efficiently unleash cross-media information without cross-media
              value. Quickly maximize timely deliverables for real-time
              schemas. Dramatically maintain clicks-and-mortar solutions
              without functional solutions.

              Completely synergize resource taxing relationships via premier
              niche markets. Professionally cultivate one-to-one customer
              service with robust ideas. Dynamically innovate
              resource-leveling customer service for state of the art customer
              service."""
  , date = "2015-01-30"
  , pinned = False
  , markedAsDone = False
  , isTruncated = True
  }
