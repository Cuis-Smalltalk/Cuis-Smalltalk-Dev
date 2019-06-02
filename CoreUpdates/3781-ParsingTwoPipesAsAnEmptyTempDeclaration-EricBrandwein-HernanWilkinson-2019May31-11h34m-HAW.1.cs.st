'From Cuis 5.0 of 7 November 2016 [latest update: #3779] on 31 May 2019 at 11:51:56 am'!

!SHParserST80 methodsFor: 'scan' stamp: 'EB 5/30/2019 19:54:12'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := c asString.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , d asString.
			self nextChar.
			^currentToken].
	(c == $| and: [d == $|])
		ifTrue: ["|| empty temp declaration"
			^currentToken].
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

