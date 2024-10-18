'From Cuis7.1 [latest update: #6783] on 18 October 2024 at 12:38:19 pm'!

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/18/2024 12:37:35'!
returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
"Disabled. If someone needs this, we could add a preference."
false ifTrue: [
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) ifTrue: [
			self addString: UnicodeString crString.
			^false ].
		self addString: UnicodeString crlfString.
		^false ].
].
	^ self newLine: aKeyboardEvent! !

