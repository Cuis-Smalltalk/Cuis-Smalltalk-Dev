'From Cuis7.7 [latest update: #7790] on 6 January 2026 at 2:57:09 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 6/Jan/2026 14:56:44'!
startKeyboardDispatch: aKeyboardEvent
	(aKeyboardEvent isEsc and: [self hasSubmorphs]) ifTrue: "give an opportunity to cancel drag with esc"
		[self submorphsReverseDo: [:each| each dragCancel].
		self hasSubmorphs ifFalse: [self showHardwareCursor].
		aKeyboardEvent wasHandled: true.
		^ self].
	aKeyboardEvent dispatchIn: (self keyboardFocus ifNil: [owner]).! !

