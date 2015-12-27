module Initial where

import Item exposing (Model, initModel)
import TimeUtil exposing (stringToTime)
import Email
import Reminder
import ItemList 
import Html exposing (..)
import Dict

--------------------------------------------------------------------------------
itemTemplate : Item.Model
itemTemplate =
  { itemModel = Item.ReminderModel (Reminder.init "placeholder reminder body")
  , date = TimeUtil.stringToTime "2015-01-30"
  , pinned = False
  , markedAsDone = False
  , isFocused = False
  , isPastDeadline = False
  , isSnoozed = False
  , snoozeDateInputValue = ""
  , snoozedUntilDate = 0
  , snoozeInputState = Dict.empty
  }

--------------------------------------------------------------------------------

itemEx1 : Item.Model
itemEx1 =
  { itemTemplate | itemModel = Item.EmailModel emailEx1 }

emailEx1 : Email.Model
emailEx1  =
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
--------------------------------------------------------------------------------
itemEx2 : Item.Model
itemEx2 =
  { itemTemplate
    | itemModel = Item.EmailModel emailEx2
    , date = TimeUtil.stringToTime "2015-09-30"
  }

emailEx2 : Email.Model
emailEx2 =
  { from = "hello@test.me"
  , to = "goodbye@test.me"
  , title = "Shorter than 200"
  , body = """This is the body of an email with less than 200 characters."""
  , isTruncated = False
  }
--------------------------------------------------------------------------------
itemEx3 : Item.Model
itemEx3 =
  { itemTemplate
    | itemModel = Item.ReminderModel reminderEx1
    , date = TimeUtil.stringToTime "2016-09-30"
    }
reminderEx1 : Reminder.Model
reminderEx1 =
  {
  body = "Take out the trash"
  }
--------------------------------------------------------------------------------
itemEx4 : Item.Model
itemEx4 =
  { itemTemplate
    | itemModel = Item.ReminderModel reminderEx2
    , date = TimeUtil.stringToTime "2015-09-25"
  }
reminderEx2 : Reminder.Model
reminderEx2 =
  { body = "Groceries"}
--------------------------------------------------------------------------------
initialItemList : ItemList.Model
initialItemList =
  { itemList = [ (1,itemEx1), (2,itemEx2), (3,itemEx3), (4, itemEx4) ]
  , nextID = 5
  , indexSelectedItem = 0
  , reversedSortingOrder = False
  , doneItemsVisible = True
  , snoozedSectionVisible = False
  }
