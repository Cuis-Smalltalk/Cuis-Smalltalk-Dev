'From Cuis7.3 [latest update: #7042] on 31 January 2025 at 4:31:39 pm'!

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 1/31/2025 16:30:41'!
handlesKeyboard

	self isLocked ifTrue: [^false ].
	self disablesEditing ifTrue: [^false ].
	((owner is: #ScrollPane) and: [owner isLocked])
		ifTrue: [^false ].
	^self visible! !

