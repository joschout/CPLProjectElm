module ItemList where

import Item exposing (Model, view)
import Html exposing (..)


filterOnDone : Model -> Bool -> Model
filterOnDone model shouldBeDone =
  let itemIsDone = \(_, item) -> item.markedAsDone
      itemIsNotDone = \(_, item) -> not item.markedAsDone
  in case shouldBeDone of
      True ->
        { model | itemList = List.filter itemIsDone model.itemList }
      False ->
        { model | itemList = List.filter itemIsNotDone model.itemList }

sortModel : Model -> Model
sortModel model =
  { model | itemList = List.sortWith sortOnItem model.itemList}

sortOnItem : (ID, Item.Model) -> (ID, Item.Model) -> Order
sortOnItem (_, itemModel1) (_, itemModel2) =
  sortItemModel itemModel1 itemModel2

{-- in sortWith worden dingen die LT zijn eerder in de lijst geplaatst.
Aangezien we de pinned items eerst willen hebben,
 wordt True hier als kleinerg gezien dan False--}
sortItemModel : Item.Model -> Item.Model -> Order
sortItemModel item1 item2 =
  let isPinned1 = item1.pinned
      isPinned2 = item2.pinned
  in case (isPinned1, isPinned2) of
    (True, False) ->
      LT
    (False, True) ->
      GT
    (_, _) ->
      sortOnDate item1 item2

sortOnDate : Item.Model -> Item.Model -> Order
sortOnDate item1 item2 =
  let dateItem1 = item1.date
      dateItem2 = item2.date
  in compare dateItem1 dateItem2

-- MODEL -----------------------------------------------------------------------
actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

type alias Model =
   { itemList : List (ID, Item.Model )
   , nextID : ID
 }

type alias ID = Int

-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp
  | ItemAction ID Item.Action

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    ItemAction idOfItem itemAction ->
      let updateItem (itemID, itemModel) =
        if itemID == idOfItem
          then (itemID, Item.update itemAction itemModel)
          else (itemID, itemModel)
      in { model | itemList = List.map updateItem model.itemList }



-- VIEW ------------------------------------------------------------------------
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewToDoDiv address model
    , viewDoneDiv address model
  --  , viewReminderDiv address model
    ]

-- VIEW TO DO SECTION
viewToDoDiv : Signal.Address Action -> Model -> Html
viewToDoDiv address model =
  let toDoModel = filterOnDone model False
  in case List.isEmpty toDoModel.itemList of
    True ->
      div [] []
    False ->
      let sortedToDoModel = sortModel toDoModel
          items = List.map (viewItem address) sortedToDoModel.itemList
      in div [] ([toDoHeader] ++ items)

viewItem : Signal.Address Action -> (ID, Item.Model) -> Html
viewItem address (id, itemModel) =
  Item.view (Signal.forwardTo address (ItemAction id)) itemModel

toDoHeader : Html
toDoHeader =
  h1 [] [text "To do"]

-- VIEW DONE SECTION
viewDoneDiv : Signal.Address Action -> Model -> Html
viewDoneDiv address model =
  let doneModel = filterOnDone model True
  in case List.isEmpty doneModel.itemList of
    True ->
      div [] []
    False ->
      let sortedDoneModel = sortModel doneModel
          items = List.map (viewItem address) sortedDoneModel.itemList
      in div [] ([doneHeader] ++ items)

doneHeader : Html
doneHeader =
  h1 [] [text "Done"]

{--
-- VIEW REMINDER SECTION
viewReminderDiv  : Signal.Address Action -> Model -> Html
viewReminderDiv address model =
  div []

reminderHeader : Html
reminderHeader =
  h1 [] [text "Reminder"]
--}
