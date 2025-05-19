'From Cuis7.3 [latest update: #7178] on 19 May 2025 at 5:05:41 pm'!

!MenuMorph methodsFor: 'modal progress dialog' stamp: 'HAW 5/19/2025 17:04:50'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		w displayWorldOn: w mainCanvas.  "show myself"
		].
	aBlock ensure: [ self delete ].! !

