'From Cuis 6.0 [latest update: #5563] on 28 November 2022 at 12:29:53 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 11:33:13'!
isValidInBinarySelectors
	"
	(UnicodeCodePoint codePoint: 16r22C7) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r042F) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r0431) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r20D7) isValidInBinarySelectors
	"
	(Character codePoint: value) ifNotNil: [ :ch | ^ch isValidInBinarySelectors ].
	^self isSymbolLike or: [ self isDiacriticalMark ].! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 11:34:00'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ `$_ numericValue` = value or: [ self isDiacriticalMark ]]! !


!Scanner methodsFor: 'expression types' stamp: 'jmv 11/28/2022 11:50:50'!
scanLitWord
	"Accumulate keywords and asSymbol the result."

	token _ (source collectionSpecies streamContents: [ :stream |
		stream nextPutAll: token.
		[	| type |
			type _ self typeTableAt: hereChar. 
			type == #xLetter or: [type == #xDiacritical] ] whileTrue: [
			self xLetter.
			stream nextPutAll: token ] ]) asSymbol! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 11/28/2022 11:40:14'!
typeTableAt: aCharacterOrCodePoint
	aCharacterOrCodePoint iso8859s15Code ifNotNil: [ :i |
		i <= typeTable size ifTrue: [
			^typeTable at: i ]].
	"For CodePoints not in the ISO 8859-15 set (i.e. not a possible instance of Character)"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isValidInBinarySelectors ifTrue: [ ^#xBinary ].
	^#xLetter! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 11/28/2022 11:51:26'!
xBinary

	tokenType _ #binary.
	token _ source collectionSpecies streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type _ self typeTableAt: hereChar.
		(type == #xBinary or: [ type == #xDiacritical ]) and: [
			hereChar ~= $- or: [aheadChar isDigit not]]
	] whileTrue: [
		stream nextPut: self step]].
	token _ token asSymbol.! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 11/28/2022 11:47:23'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type _ self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore
		or: [type == #xDiacritical]]]] whileTrue: [
			"open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
				ifTrue: [DoItCharacter "doit"]
				ifFalse: [source next]].
	tokenType _ (type == #colon or: [type == #xColon and: [aheadChar ~= $=]])
		ifTrue: [
			buffer nextPut: self step.
			"Allow any number of embedded colons in literal symbols"
			[(self typeTableAt: hereChar) == #xColon]
				whileTrue:
					[buffer nextPut: self step].
			#keyword]
		ifFalse: [
			#word].
	token _ buffer contents.! !

