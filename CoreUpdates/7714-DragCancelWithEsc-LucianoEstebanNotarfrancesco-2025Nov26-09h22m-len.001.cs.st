'From Cuis7.5 [latest update: #7710] on 26 November 2025 at 9:28:46 am'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'len 11/17/2025 14:25:54'!
dragCancel
	"This message is sent if the user presses ESC while the hand is dragging this morph, to give this morph an opportunity to cancel the dragging. By default do nothing.
	For example, a morph might want to handle this by deleting itself from the hand, or by returning to its original owner and position."
	^ self! !


!DraggingGuideMorph methodsFor: 'dropping/grabbing' stamp: 'len 11/14/2025 14:33:11'!
dragCancel
	self delete! !


!HandMorph methodsFor: 'events-processing' stamp: 'len 11/15/2025 18:07:02'!
startKeyboardDispatch: aKeyboardEvent
	(aKeyboardEvent isEsc and: [self hasSubmorphs]) ifTrue: "give an opportunity to cancel drag with esc"
		[self submorphsReverseDo: [:each| each dragCancel].
		self hasSubmorphs ifFalse: [self showHardwareCursor].
		aKeyboardEvent wasHandled: true.
		^ self].
	aKeyboardEvent sendEventTo: (self keyboardFocus ifNil: [self world])! !

