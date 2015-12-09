'From Cuis 4.2 of 25 July 2013 [latest update: #2626] on 8 December 2015 at 8:41:58.553062 pm'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 12/8/2015 20:40'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the composition"

	self selectionChanged.  "Note old selection"

	interactionBlock value.

	self selectionChanged.  "Note new selection"
	self updateFromTextComposition! !

