module ItemExtraction where

import Item
import JSONUtil exposing (EmailKVString)
import Static

-- EXTRACTING ITEMS FROM STATIC ------------------------------------------------
getRemindersFromStatic : List Item.Model
getRemindersFromStatic =
  List.map staticReminderToItem Static.reminders

getEmailsFromStatic : List Item.Model
getEmailsFromStatic =
  List.map staticEmailToItem Static.emails

staticReminderToItem : Static.Reminder -> Item.Model
staticReminderToItem staticReminder =
  Item.newReminderItem staticReminder.body staticReminder.created False False
  -- newReminderItem body' date' pinned' markedAsDone'

staticEmailToItem : Static.Email -> Item.Model
staticEmailToItem staticEmail =
  Item.newEmailItem staticEmail.from staticEmail.to staticEmail.title staticEmail.date staticEmail.body
  -- newEmailItem fromValue toValue titleValue dateValue bodyValue

-- EXTRACTING ITEMS FROM JSON --------------------------------------------------
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
