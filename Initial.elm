module Initial where

import Item exposing (Model)
import TimeUtil exposing (stringToTime)
import Email
import Reminder
import ItemList exposing (actionMailbox)
import Html exposing (..)
--------------------------------------------------------------------------------
itemEx1 : Item.Model
itemEx1=
  { itemModel = Item.EmailModel emailEx1
    --itemModel = ReminderModel Reminder.initModel
  , date = TimeUtil.stringToTime "2015-01-30"
  , pinned = False
  , markedAsDone = False
  }

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
  { itemModel = Item.EmailModel emailEx2
    --itemModel = ReminderModel Reminder.initModel
  , date = TimeUtil.stringToTime "2015-09-30"
  , pinned = False
  , markedAsDone = False
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
    { itemModel = Item.ReminderModel reminderEx1
      --itemModel = ReminderModel Reminder.initModel
    , date = TimeUtil.stringToTime "2016-09-30"
    , pinned = False
    , markedAsDone = False
    }
reminderEx1 : Reminder.Model
reminderEx1 =
  {
  body = "Take out the trash"
  }
--------------------------------------------------------------------------------
itemEx4 : Item.Model
itemEx4 =
  { itemModel = Item.ReminderModel reminderEx2
  , date = TimeUtil.stringToTime "2015-09-25"
  , pinned = False
  , markedAsDone = False
  }
reminderEx2 : Reminder.Model
reminderEx2 =
  { body = "Groceries"}
--------------------------------------------------------------------------------
initialItemList : ItemList.Model
initialItemList =
  { itemList = [ (1,itemEx1), (2,itemEx2), (3,itemEx3), (4, itemEx4) ]
  , nextID = 5
  }
