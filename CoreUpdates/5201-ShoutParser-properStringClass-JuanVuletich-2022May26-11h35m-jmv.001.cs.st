'From Cuis 6.0 [latest update: #5203] on 26 May 2022 at 11:35:46 am'!

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/24/2022 11:20:42'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := source class with: c.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c = $:]) ifFalse: [^currentToken].
	(c = $: and: [d = $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , (source class with: d).
			self nextChar.
			^currentToken].
	"|| empty temp declaration"
	"This makes || colorize as an invalid method definition.
	It is better to allow #|| as valid selector, and mark an empty temp declaration as invalid instead!!"
	"(c = $| and: [d = $|])
		ifTrue: [^currentToken]."
	c _ d.
	[
		d _ self peekChar.
		c = $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken _ currentToken copyWith: c.
			c _ self nextChar ].
	^currentToken! !

