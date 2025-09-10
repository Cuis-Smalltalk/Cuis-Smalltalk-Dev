'From Cuis7.5 [latest update: #7538] on 10 September 2025 at 11:11:49 am'!

!Morph methodsFor: 'events' stamp: 'jmv 9/10/2025 11:09:28'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition

	aMouseButtonEvent shiftPressed
		ifTrue: [self addHalo: aMouseButtonEvent ]
		ifFalse: [self root addHalo: aMouseButtonEvent].! !

