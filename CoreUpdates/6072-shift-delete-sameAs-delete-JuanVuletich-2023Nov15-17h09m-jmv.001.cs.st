'From Cuis 6.0 [latest update: #6071] on 15 November 2023 at 5:13:03 pm'!

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 11/15/2023 17:12:37'!
backspace: aKeyboardEvent
	"Backspace over the last character."
	"This is a user command, and generates undo"

	| startIndex |
	(aKeyboardEvent rawMacOptionKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [ ^ self backWord: aKeyboardEvent ].
	startIndex := self markIndex + (self hasSelection ifTrue: [1] ifFalse: [0]).
	startIndex := 1 max: startIndex - 1.
	self backTo: startIndex.
	^ false! !

