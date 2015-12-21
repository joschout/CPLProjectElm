module ItemList where

import Item exposing (Model)


filterOnDone : Model -> Bool -> Model
filterOnDone model shouldBeDone =
  case shouldBeDone of
    True ->
      { model | itemList = filter ( _, item -> markedAsDone) model.itemList }
    False ->
      { model | itemList = filter ( _, item -> (not markedAsDone)) model.itemList }

{-- in sortWith worden dingen die LT zijn eerder in de lijst geplaatst.
Aangezien we de pinned items eerst willen hebben,
 wordt True hier als kleinerg gezien dan False--}
sortOrder : Item.Model -> Item.Model -> Order
sortOrder item1 item2 =
  let isPinned1 = item1.pinned
      isPinned2 = item2.pinned
  in case (isPinned1, isPinned2) of
    (True, False) ->
      LT
    (False, True) ->
      GT
    (_, _) ->


-- MODEL
type alias Model =
   { itemList : List (ID, Item.Model )
   , nextID : ID
 }

type alias ID = Int



-- UPDATE
type Action =
  Item Num Item.Action

update : Action -> Model -> Model
update action model =

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewToDoDiv address model
    , viewDoneDiv address model
    , viewReminderDiv address model
    ]

viewToDoDiv : Signal.Address Action -> Model -> Html
viewToDoDiv address model =

viewDoneDiv : Signal.Address Action -> Model -> Html
viewDoneDiv address model =

viewRemindeDiv  : Signal.Address Action -> Model -> Html
viewReminderDiv address model =
