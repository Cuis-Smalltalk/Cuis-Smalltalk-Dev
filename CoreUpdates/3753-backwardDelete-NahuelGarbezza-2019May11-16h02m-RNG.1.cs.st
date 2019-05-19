'From Cuis 5.0 of 7 November 2016 [latest update: #3741] on 12 May 2019 at 3:41:55 pm'!
"Change Set:		3742-CuisCore-AuthorName-2019May11-16h02m
Date:			12 May 2019
Author:			Nahuel Garbezza

Allow to delete words using ctrl/alt+backspace"!


!Editor methodsFor: 'typing/selecting keys' stamp: 'RNG 5/11/2019 16:34:48'!
backspace: aKeyboardEvent
	"Backspace over the last character."
	"This is a user command, and generates undo"

	| startIndex |
	(aKeyboardEvent rawMacOptionKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [ ^ self backWord: aKeyboardEvent ].
	aKeyboardEvent shiftPressed
		ifTrue: [ ^ self forwardDelete: aKeyboardEvent ].
	startIndex _ self markIndex + (self hasSelection ifTrue: [1] ifFalse: [0]).
	startIndex _ 1 max: startIndex - 1.
	self backTo: startIndex.
	^ false! !

