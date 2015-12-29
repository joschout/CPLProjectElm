module ItemExtraction where

import Item
import JSONUtil exposing (EmailKVString)




-- EXTRACTING ITEMS FROM JASON -------------------------------------------------
parseEmailListFromJSON : List( EmailKVString ) -> List Item.Model
parseEmailListFromJSON emailKVList =
  List.map parseSingleEmailFromJSON emailKVList

parseSingleEmailFromJSON : List(String, String) -> Item.Model
parseSingleEmailFromJSON singleEmailList =
  let fromValue = getValueFromIndexInList 4 singleEmailList
      toValue = getValueFromIndexInList 3 singleEmailList
      titleValue = getValueFromIndexInList 2 singleEmailList
      dateValue = getValueFromIndexInList 1 singleEmailList
      bodyValue = getValueFromIndexInList 0 singleEmailList
  in Item.newEmailItem fromValue toValue titleValue dateValue bodyValue

-- Takes the (sting, string)-tuple at the given index in the given list
-- and returns the ID.
getValueFromIndexInList : Int -> List(String, String) -> String
getValueFromIndexInList index list =
  let listElem = List.head
      <| List.reverse
      <| List.take (index + 1) list
  in case listElem of
    Just (_, value) ->
      value
    Nothing ->
      ""
