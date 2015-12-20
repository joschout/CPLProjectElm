module ItemList where

import Email
import Reminder

type alias ItemElem =
   Reminder | Email

type Item =
  { done : Bool
  ,  itemId : Num
  , item : Item
  }

-- MODEL
type alias Model =
   List Item


-- UPDATE
type Action =
  Item Num Item.Action

-- VIEW
