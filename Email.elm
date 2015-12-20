module Email where

import Html exposing (..)
import Html.Events exposing (onClick)
import String

type alias Email =
  { from: String
  , to: String
  , title: String
  , body: String
  , isTruncated : Bool
  }

-- MODEL
type alias Model = Email

-- UPDATE
type Action
  = NoOp
  | Truncate Bool

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Truncate truncateBool ->
      { model | isTruncated = truncateBool }

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewEmailHeading model
    , viewEmailBody model
    , viewMoreButton address model
    ]

viewEmailHeading : Model -> Html
viewEmailHeading model =
  p
    []
    [ model.title ++ " | " ++ model.from ++ " says:"
      |> text ]

viewEmailBody : Model -> Html
viewEmailBody model =
  let canBeTruncated = allowedToBeTruncated model.body
  in case canBeTruncated && model.isTruncated of
    True ->
      p
        []
        [  (String.left 200 model.body) ++ "..."
          |> text ]
    False ->
      p [] [ text model.body]

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
  , isTruncated = True
  }
