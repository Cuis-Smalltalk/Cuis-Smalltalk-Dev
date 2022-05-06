'From Cuis 6.0 [latest update: #5139] on 6 May 2022 at 10:51:54 am'!

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 5/6/2022 10:51:14'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := String with: c.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , (String with: d).
			self nextChar.
			^currentToken].
	"|| empty temp declaration"
	"This makes || colorize as an invalid method definition.
	It is better to allow #|| as valid selector, and mark an empty temp declaration as invalid instead!!"
	"(c == $| and: [d == $|])
		ifTrue: [^currentToken]."
	c _ d.
	[
		d _ self peekChar.
		c == $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken _ currentToken copyWith: c.
			c _ self nextChar ].
	^currentToken! !

