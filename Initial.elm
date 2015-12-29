module Initial  where

{-- Contains the function which gives the initial static ItemList at startup
Note that this code cannot be in ItemList or ItemExtraction,
since ther would otherwise be a circular dependency.
--}

import ItemList
import ItemExtraction

initialItemList : ItemList.Model
initialItemList =
  let initialItems = ItemExtraction.getRemindersFromStatic
                      ++ ItemExtraction.getEmailsFromStatic
      itemListModel = List.foldr ItemList.addItemToModel ItemList.emptyModel initialItems
  in ItemList.update (ItemList.ChangeFocus 0) itemListModel
