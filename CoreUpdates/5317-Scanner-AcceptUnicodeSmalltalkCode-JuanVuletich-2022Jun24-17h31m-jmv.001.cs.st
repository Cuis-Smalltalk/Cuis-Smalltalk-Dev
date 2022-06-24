'From Cuis 6.0 [latest update: #5316] on 24 June 2022 at 5:34:16 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 6/24/2022 17:33:30'!
typeTableAt: aCharacterOrCodePoint
	aCharacterOrCodePoint iso8859s15Code ifNotNil: [ :i |
		i <= typeTable size ifTrue: [
			^typeTable at: i ]].
	"For CodePoints not in the ISO 8859-15 set (i.e. not a possible instance of Character)"
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isValidInBinarySelectors ifTrue: [ ^#xBinary ].
	^#xLetter! !

