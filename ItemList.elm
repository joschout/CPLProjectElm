module ItemList where

import Item exposing (Model, view, newReminderItem)
import Html exposing (..)
import Html.Attributes exposing (style)

-- MODEL -----------------------------------------------------------------------
type alias Model =
   { itemList : List (ID, Item.Model )
   , nextID : ID
   , indexSelectedItem : Int
    -- index of the selected item in the ordered list
   , reversedSortingOrder : Bool
 }

type alias ID = Int

-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp
  | ItemAction ID Item.Action
  | AddReminder String String Bool Bool -- body' date' pinned' markedAsDone
  | ChangeFocus Int
  | ReverseSortingOrder Bool

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
          updatedModel = { model | itemList = List.map updateItem model.itemList }
      in changeFocusOfModel model.indexSelectedItem updatedModel

    AddReminder body date pinned markedAsDone ->
      let newReminder = Item.newReminderItem body date pinned markedAsDone
          newItem = (model.nextID, newReminder)
          newItemList = model.itemList ++ [ newItem ]
      in { model |
            itemList = newItemList,
            nextID = model.nextID + 1}

    ChangeFocus newIndex ->
      changeFocusOfModel newIndex model

    ReverseSortingOrder shouldBeReversed ->
      let reversedModel = { model | reversedSortingOrder = shouldBeReversed }
      in changeFocusOfModel model.indexSelectedItem reversedModel


-- CHANGING FOCUS --------------------------------------------------------------
changeFocusOfModel : Int -> Model -> Model
changeFocusOfModel newIndex model =
  let
      sortedToDoModel = sortModel
                        <| filterOnDone model False
      sortedDoneModel = sortModel
                        <| filterOnDone model True
      totalSortedList = sortedToDoModel.itemList ++ sortedDoneModel.itemList
      idNewFocusedItem = getIdFromIndex newIndex totalSortedList
  in { model | indexSelectedItem = newIndex,
               itemList = List.map (changeFocusOfItem idNewFocusedItem) model.itemList}



changeFocusOfItem : ID -> (ID, Item.Model) -> (ID, Item.Model)
changeFocusOfItem correctID (itemID, itemModel) =
  case correctID == itemID of
    True ->
      (itemID, (Item.update (Item.Focus True) itemModel))
    False ->
      (itemID, (Item.update (Item.Focus False) itemModel))

getIdFromIndex : Int -> List(ID, Item.Model) -> ID
getIdFromIndex index list =
  let listElem = List.head
      <| List.reverse
      <| List.take (index + 1) list
  in case listElem of
    Just (id', _) ->
      id'
    Nothing ->
      -1

------ EXTRA INTERFACE TO EXTERN -----------------------------------------------
getNextIndex : Model -> Int
getNextIndex model =
  let currentIndex = model.indexSelectedItem
      listLength = List.length model.itemList
      nextIndex = currentIndex + 1
  in if nextIndex > (listLength - 1) then 0
      else nextIndex
getPreviousIndex : Model -> Int
getPreviousIndex model =
  let currentIndex = model.indexSelectedItem
      listLength = List.length model.itemList
      previousIndex = currentIndex - 1
  in if previousIndex < 0 then listLength - 1
      else previousIndex

focusOnNextItemAction : Model -> Action
focusOnNextItemAction model =
  ChangeFocus (getNextIndex model)

focusOnPreviousItemAction : Model -> Action
focusOnPreviousItemAction model =
  ChangeFocus (getPreviousIndex model)

{--changeSortingAction : Model -> Action
changeSortingAction model =
  case model.reversedSortingOrder of
    True ->
      ReverseSortingOrder False
    False ->
      ReverseSortingOrder True--}

normalSortingAction : Model -> Action
normalSortingAction model =
      ReverseSortingOrder False

reverseSortingAction : Model -> Action
reverseSortingAction model =
      ReverseSortingOrder True



-- VIEW ------------------------------------------------------------------------
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewToDoDiv address model
    , viewDoneDiv address model
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
      in div [ itemListStyle ] ([toDoHeader] ++ items)

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
      in div [ itemListStyle ] ([doneHeader] ++ items)

doneHeader : Html
doneHeader =
  h1 [] [text "Done"]

-- SIGNALS --------------------------------------------------------------------

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actionSignal : Signal Action
actionSignal = actionMailbox.signal

-- FILTERING & SORTING ---------------------------------------------------------

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
  { model | itemList
              = List.sortWith (sortOnItem model.reversedSortingOrder) model.itemList }

sortOnItem : Bool -> (ID, Item.Model) -> (ID, Item.Model) -> Order
sortOnItem isReversed (_, itemModel1) (_, itemModel2) =
  sortItemModel isReversed itemModel1 itemModel2

{-- in sortWith worden dingen die LT zijn eerder in de lijst geplaatst.
Aangezien we de pinned items eerst willen hebben,
 wordt True hier als kleinerg gezien dan False--}
sortItemModel : Bool -> Item.Model -> Item.Model -> Order
sortItemModel isReversed item1 item2 =
  case isReversed of
    False ->
      let isPinned1 = item1.pinned
          isPinned2 = item2.pinned
      in case (isPinned1, isPinned2) of
        (True, False) ->
          LT
        (False, True) ->
          GT
        (_, _) ->
          sortOnDate isReversed item1 item2
    True ->
      sortOnDate isReversed item1 item2

sortOnDate : Bool -> Item.Model -> Item.Model -> Order
sortOnDate isReversed item1 item2 =
  let dateItem1 = item1.date
      dateItem2 = item2.date
  in case isReversed of
    False ->
      compare dateItem2 dateItem1
    True ->
      compare dateItem1 dateItem2

-- STYLE -----------------------------------------------------------------------
itemListStyle : Attribute
itemListStyle =
  style
    [ ("width", "40%")
    , ("margin", "auto")
    ]
