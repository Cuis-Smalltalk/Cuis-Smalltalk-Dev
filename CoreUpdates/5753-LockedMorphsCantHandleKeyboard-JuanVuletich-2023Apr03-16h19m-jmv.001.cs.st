'From Cuis 6.0 [latest update: #5752] on 3 April 2023 at 4:32:18 pm'!

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 4/3/2023 16:29:59'!
handlesKeyboard

	self isLocked ifTrue: [^false ].
	((owner is: #ScrollPane) and: [owner isLocked])
		ifTrue: [^false ].
	^self visible! !


!HierarchicalListMorph methodsFor: 'event handling testing' stamp: 'jmv 4/3/2023 16:30:23'!
handlesKeyboard

	self isLocked ifTrue: [^false ].
	^self visible! !


!PluggableListMorph methodsFor: 'event handling testing' stamp: 'jmv 4/3/2023 16:30:26'!
handlesKeyboard

	self isLocked ifTrue: [^false ].
	^self visible! !


!MenuMorph methodsFor: 'event handling testing' stamp: 'jmv 4/3/2023 16:30:29'!
handlesKeyboard
	"Answer whether the receiver handle keyboard events"

	self isLocked ifTrue: [^false ].
	^self visible! !

