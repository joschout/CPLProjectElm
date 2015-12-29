module Email
  ( Model, Action (..) , update, view, equal, init) where
{--
The Email module contains the logic for Email items.
--}
import Html exposing (..)
import Html.Events exposing (onClick)
import String

-- MODEL -----------------------------------------------------------------------
type alias Model =
  { from: String
  , to: String
  , title: String
  , body: String
  , isTruncated : Bool
  }

-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp
  | ToggleTruncation

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    ToggleTruncation ->
      toggleTruncation model

toggleTruncation : Model -> Model
toggleTruncation model =
  let canBeTruncated = allowedToBeTruncated model.body
  in case canBeTruncated of
    True ->
      { model | isTruncated = (not model.isTruncated) }
    False ->
      model

-- VIEW ------------------------------------------------------------------------
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
        [ onClick address ToggleTruncation ]
        [ text <| moreButtonText model ]
    False ->
      div [] []

moreButtonText : Model -> String
moreButtonText model =
  case model.isTruncated of
    True
      -> "More"
    False
      -> "Less"
-- UTILS -----------------------------------------------------------------------
allowedToBeTruncated : String -> Bool
allowedToBeTruncated string =
  let stringLength = String.length string
  in if stringLength > 200  then True
    else False

equal : Model -> Model -> Bool
equal email1 email2 =
  let fromEq = email1.from == email2.from
      toEq = email1.to == email2.to
      titleEq = email1.title == email2.title
      bodyEq = email1.body == email2.body
  in fromEq && toEq && titleEq && bodyEq

init : String ->  String -> String -> String -> Model
init from' to' title' body' =
  { from = from'
  , to = to'
  , title = title'
  , body = body'
  , isTruncated = True
  }
