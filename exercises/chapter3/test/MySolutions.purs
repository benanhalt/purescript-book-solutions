module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.Maybe (Maybe)
import Data.List (head, filter, null, nubByEq)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry = ((==) street) <<< _.address.street


isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not <<< null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq sameNames
  where
    sameNames :: Entry -> Entry -> Boolean
    sameNames entry1 entry2 = entry1.firstName == entry2.firstName
                              && entry1.lastName == entry2.lastName
