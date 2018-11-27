'From Cuis 5.0 of 7 November 2016 [latest update: #3496] on 27 November 2018 at 9:21:11 am'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 11/26/2018 18:20:34'!
selectionChanged

	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].
	editor hasSelection
		ifTrue: [ self stopBlinking ]
		ifFalse: [ self hasKeyboardFocus ifTrue: [self startBlinking ]]! !

