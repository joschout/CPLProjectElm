module ItemList
  ( Model, view, update, Action(..)
  , focusOnPreviousItemAction, focusOnNextItemAction
  , normalSortingAction, reverseSortingAction
  , toggleTruncationAction, togglePinnedAction, toggleDoneAction
  ) where

import Item exposing (Model, view, newReminderItem, toggleTruncation, Action)
import Html exposing (..)
import Html.Attributes exposing (style)
import Time exposing (Time)

-- MODEL -----------------------------------------------------------------------
type alias Model =
   { itemList : List (ID, Item.Model)
   , nextID : ID
   , indexSelectedItem : Int
    -- index of the selected item in the ordered list
   , reversedSortingOrder : Bool
   , doneItemsVisible : Bool
   , snoozedSectionVisible : Bool
 }

type alias ID = Int

-- UPDATE ----------------------------------------------------------------------
type Action
  = NoOp
  | ItemAction ID Item.Action
  | AddReminder String String Bool Bool -- body' date' pinned' markedAsDone
  | ChangeFocus Int
  | ReverseSortingOrder Bool
  -- EXTENSIONS
  | ToggleVisiblityDoneItems
  --
  | CheckDeadlines Time
  --
  | CheckSnoozeTimes Time
  | ToggleVisibilitySnoozedSection

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

    ToggleVisiblityDoneItems ->
      let modelToggledVisibility = { model | doneItemsVisible = (not model.doneItemsVisible) }
          currentIndex = model.indexSelectedItem
          totalListLength = totalListLengthOfVisibleSections modelToggledVisibility
          currentIndexMod = currentIndex % totalListLength
      in { modelToggledVisibility | indexSelectedItem = currentIndexMod}
                                  --, itemList = List.map (changeFocusOfItem currentIndexMod) model.itemList}

    CheckDeadlines currentTime ->
      let updatedItemList
            = List.map (checkAndUpdateDeadline currentTime) model.itemList
      in { model | itemList = updatedItemList }

    CheckSnoozeTimes currentTime ->
      let updatedItemList
          = List.map (checkSnoozeTimes currentTime) model.itemList
      in { model | itemList = updatedItemList }

    ToggleVisibilitySnoozedSection ->
      let modelToggledVisibility = { model | snoozedSectionVisible = (not model.snoozedSectionVisible) }
          currentIndex = model.indexSelectedItem
          totalListLength = totalListLengthOfVisibleSections modelToggledVisibility
          currentIndexMod = currentIndex % totalListLength
      in { modelToggledVisibility | indexSelectedItem = currentIndexMod }
                                  --  , itemList = List.map (changeFocusOfItem currentIndexMod) model.itemList }

checkAndUpdateDeadline : Time -> (ID, Item.Model) -> (ID, Item.Model)
checkAndUpdateDeadline currentTime (id, itemModel) =
  (id, (Item.update (Item.CheckDeadline currentTime) itemModel))

checkSnoozeTimes : Time -> (ID, Item.Model) -> (ID, Item.Model)
checkSnoozeTimes currentTime (id, itemModel) =
  (id, (Item.update (Item.CheckSnoozeTime currentTime) itemModel))

-- CHANGING FOCUS --------------------------------------------------------------
changeFocusOfModel : Int -> Model -> Model
changeFocusOfModel newIndex model =
  let idNewFocusedItem
        = getIdFromIndexInUnsortedModelList newIndex model
  in { model | indexSelectedItem = newIndex,
               itemList = List.map (changeFocusOfItem idNewFocusedItem) model.itemList}

changeFocusOfItem : ID -> (ID, Item.Model) -> (ID, Item.Model)
changeFocusOfItem correctID (itemID, itemModel) =
  case correctID == itemID of
    True ->
      (itemID, (Item.update (Item.Focus True) itemModel))
    False ->
      (itemID, (Item.update (Item.Focus False) itemModel))

getIdFromIndexInUnsortedModelList : Int -> Model -> ID
getIdFromIndexInUnsortedModelList index model =
  let sortedToDoModel = sortModel
                      <| getToDoModelNotSnoozed model
      sortedDoneItemsList =
        case model.doneItemsVisible of
          True ->
            let sortedDoneModel = sortModel
                      <| getDoneModelNotSnoozed model
            in sortedDoneModel.itemList
          False ->
            []
      snoozedItemsList =
        case model.snoozedSectionVisible of
          True ->
            let snoozedModel = filterOnSnoozed model True
            in snoozedModel.itemList
          False ->
            []
      totalSortedList = sortedToDoModel.itemList ++ sortedDoneItemsList ++ snoozedItemsList
  in getIdFromIndexInSortedList index totalSortedList


getIdFromIndexInSortedList : Int -> List(ID, Item.Model) -> ID
getIdFromIndexInSortedList index list =
  let listElem = List.head
      <| List.reverse
      <| List.take (index + 1) list
  in case listElem of
    Just (id', _) ->
      id'
    Nothing ->
      -1

--------------------------------------------------------------------------------s
getNextIndex : Model -> Int
getNextIndex model =
  let currentIndex = model.indexSelectedItem
      totalListLength = totalListLengthOfVisibleSections model
      nextIndex = currentIndex + 1
  in nextIndex % totalListLength

getPreviousIndex : Model -> Int
getPreviousIndex model =
  let currentIndex = model.indexSelectedItem
      totalListLength = totalListLengthOfVisibleSections model
      previousIndex = currentIndex - 1
  in  previousIndex % totalListLength

totalListLengthOfVisibleSections : Model -> Int
totalListLengthOfVisibleSections model =
  let numberOfToDoItems = numberOfItems <| getToDoModelNotSnoozed model
      numberOfDoneItems =
        case model.doneItemsVisible of
          True ->
            numberOfItems <| getDoneModelNotSnoozed model
          False ->
            0
      numberOfSnoozedItems =
        case model.snoozedSectionVisible of
          True ->
            numberOfItems <| filterOnSnoozed model True
          False ->
            0
  in numberOfToDoItems + numberOfDoneItems + numberOfSnoozedItems

numberOfItems : Model -> Int
numberOfItems model =
  let listOfItems = model.itemList
  in List.length listOfItems

getDoneModelNotSnoozed : Model -> Model
getDoneModelNotSnoozed model =
  let notSnoozedModel = filterOnSnoozed model False
  in  filterOnDone notSnoozedModel True

getToDoModelNotSnoozed : Model -> Model
getToDoModelNotSnoozed model =
  let notSnoozedModel = filterOnSnoozed model False
  in  filterOnDone notSnoozedModel False



-- EXTRA INTERFACE TO EXTERN ---------------------------------------------------
focusOnNextItemAction : Model -> Action
focusOnNextItemAction model =
  ChangeFocus (getNextIndex model)

focusOnPreviousItemAction : Model -> Action
focusOnPreviousItemAction model =
  ChangeFocus (getPreviousIndex model)

normalSortingAction : Model -> Action
normalSortingAction model =
  ReverseSortingOrder False

reverseSortingAction : Model -> Action
reverseSortingAction model =
  ReverseSortingOrder True

toggleTruncationAction : Model -> Action
toggleTruncationAction model =
  let idSelecteditem
      = getIdFromIndexInUnsortedModelList model.indexSelectedItem model
  in ItemAction idSelecteditem Item.toggleTruncation

togglePinnedAction : Model -> Action
togglePinnedAction model =
  let idSelecteditem
      = getIdFromIndexInUnsortedModelList model.indexSelectedItem model
  in ItemAction idSelecteditem Item.TogglePinned

toggleDoneAction : Model -> Action
toggleDoneAction model =
  let idSelecteditem
      = getIdFromIndexInUnsortedModelList model.indexSelectedItem model
  in ItemAction idSelecteditem Item.ToggleMarkedAsDone



-- VIEW ------------------------------------------------------------------------
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewToDoDiv address model
    , viewDoneDiv address model
    , viewSnoozeSection address model
    ]

-- VIEW TO DO SECTION
viewToDoDiv : Signal.Address Action -> Model -> Html
viewToDoDiv address model =
  let toDoModel = getToDoModelNotSnoozed model
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
  let doneModel = getDoneModelNotSnoozed model
  in case shouldDoneItemsBeShown doneModel of
    False ->
      div [] []
    True ->
      let sortedDoneModel = sortModel doneModel
          items = List.map (viewItem address) sortedDoneModel.itemList
      in div [ itemListStyle ] ([doneHeader] ++ items)

shouldDoneItemsBeShown : Model -> Bool
shouldDoneItemsBeShown doneModel =
  let doneListNotEmpty = not (List.isEmpty doneModel.itemList)
      doneSectionVisible = doneModel.doneItemsVisible
  in doneListNotEmpty && doneSectionVisible

doneHeader : Html
doneHeader =
  h1 [] [text "Done"]

-- VIEW SNOOZED SECTION
viewSnoozeSection : Signal.Address Action -> Model -> Html
viewSnoozeSection address model =
  case model.snoozedSectionVisible of
    True ->
      let snoozedModel = filterOnSnoozed model True
          items = List.map (viewItem address) snoozedModel.itemList
      in div [ itemListStyle ]  ([snoozedHeader] ++ items)
    False ->
      div [] []

snoozedHeader : Html
snoozedHeader =
  h1 [] [text "Snoozed"]

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


filterOnSnoozed : Model -> Bool -> Model
filterOnSnoozed model  shouldBeSnoozed =
  let itemIsSnoozed = \(_, item) -> item.isSnoozed
      itemIsNotSnoozed = \(_, item) -> not (item.isSnoozed)
  in case shouldBeSnoozed of
    True ->
      { model | itemList = List.filter itemIsSnoozed model.itemList }
    False ->
      { model | itemList = List.filter itemIsNotSnoozed model.itemList }

-- STYLE -----------------------------------------------------------------------
itemListStyle : Attribute
itemListStyle =
  style
    [ ("width", "40%")
    , ("margin", "auto")
    ]
