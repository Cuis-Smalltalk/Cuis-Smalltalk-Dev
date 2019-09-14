'From Cuis 5.0 [latest update: #3870] on 12 September 2019 at 1:04:57 pm'!

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/12/2019 13:03:45'!
returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
	aKeyboardEvent commandAltKeyPressed print ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) print ifTrue: [
			self addString: String crString.
			^false ].
		self addString: String crlfString.
		^false ].
	^ self newLine: aKeyboardEvent! !

