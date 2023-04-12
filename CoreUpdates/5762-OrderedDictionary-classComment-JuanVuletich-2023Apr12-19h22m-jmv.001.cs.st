'From Cuis 6.0 [latest update: #5761] on 12 April 2023 at 7:23:13 pm'!

!OrderedDictionary commentStamp: '<historical>' prior: 0!
My instances are very much like Dictionaries.
The only difference is that they remember the order in which keys were added, and iterators like #do:, #keysDo: and #associationsDo: will do the iteration in that deterministic order.!

