'From Cuis 5.0 [latest update: #4825] on 1 September 2021 at 12:19:43 pm'!

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2021 12:19:27'!
calculateItemsPerPage

	| itemsThatCanFit |

	itemsThatCanFit _ (Display height - originalPosition y - 2) // self itemHeight.
	itemsPerPage _ (itemsThatCanFit min: self maxItemsPerPage) min: self entryCount.

! !

