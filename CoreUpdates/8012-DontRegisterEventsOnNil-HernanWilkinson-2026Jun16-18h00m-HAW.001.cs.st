'From Cuis7.9 [latest update: #8001] on 17 June 2026 at 11:34:33 am'!

!DebuggerWindow methodsFor: 'initialization' stamp: 'HAW 16/Jun/2026 18:18:18'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.
	model ifNotNil: [ model when: #closeViews send: #closeView to: self ].! !

!PreDebugWindow methodsFor: 'initialization' stamp: 'HAW 16/Jun/2026 18:18:18'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.
	model ifNotNil: [ model when: #closeViews send: #closeView to: self ].! !

