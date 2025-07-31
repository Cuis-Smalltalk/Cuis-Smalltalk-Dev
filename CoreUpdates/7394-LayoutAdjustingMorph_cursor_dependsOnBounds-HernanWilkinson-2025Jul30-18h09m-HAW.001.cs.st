'From Cuis7.5 [latest update: #7389] on 31 July 2025 at 3:49:04 pm'!

!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'HAW 7/31/2025 15:44:18'!
cursor

	^self localBounds isTall 
		ifTrue: [ Cursor cursorAt: #resizeLeftCursor ]
		ifFalse: [ Cursor cursorAt: #resizeTopCursor ].! !

