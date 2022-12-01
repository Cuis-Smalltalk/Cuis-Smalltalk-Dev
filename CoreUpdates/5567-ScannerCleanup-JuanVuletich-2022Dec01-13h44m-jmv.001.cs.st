'From Cuis 6.0 [latest update: #5566] on 1 December 2022 at 1:47:57 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:53:24'!
isNullCharacter
	"Answer whether the receiver is the NULL character."

	^ self numericValue = 0! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:53:35'!
isNullCharacter
	"Answer whether the receiver is the NULL character."

	^ value = 0! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:36:20'!
typeTable: aTypeTable at: aCharacterOrCodePoint
	aCharacterOrCodePoint iso8859s15Code ifNotNil: [ :i |
		i <= aTypeTable size ifTrue: [
			^aTypeTable at: i ]].
	"For CodePoints not in the ISO 8859-15 set (i.e. not a possible instance of Character)"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isValidInBinarySelectors ifTrue: [ ^#xBinary ].
	^#xLetter! !

!Scanner class methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:39:36'!
typeTableAt: aCharacterOrCodePoint
	TypeTable ifNil: [
		self initTypeTable ].
	^self typeTable: TypeTable at: aCharacterOrCodePoint! !


!Character methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:57:12'!
isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	"
	self isNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary) statePointsTo: (Scanner typeTableAt: self).! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 12/1/2022 11:36:45'!
typeTableAt: aCharacterOrCodePoint
	^self class typeTable: typeTable at: aCharacterOrCodePoint! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 12/1/2022 11:54:48'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| i char type |
	i := aSymbol size.
	i = 0 ifTrue: [^ false].
	char := aSymbol at: 1.
	"TypeTable should have been origined at 0 rather than 1 ..."
	char isNullCharacter ifTrue: [^ false].
	type := self typeTableAt: char.
	type == #xBinary ifTrue:
		[^i <= 2
		  and: [i = 1 or: [(self typeTableAt: (aSymbol at: 2)) == #xBinary]]].
	(type == #xColon or: [type == #verticalBar]) ifTrue:
		[^i = 1].
	(type == #xLetter or: [type == #xUnderscore]) ifTrue: [
		[i > 1] whileTrue: [
			char := aSymbol at: i.
			char isNullCharacter ifTrue: [^ false].
			type := self typeTableAt: char.
			((type == #xLetter or: [type == #xUnderscore])or: [type == #xDigit or: [type == #xColon]]) ifFalse: [
				^false].
			i := i - 1].
		^true].
	^false! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 12/1/2022 11:51:31'!
peekChar
	^source at: sourcePosition + 1 ifAbsent: [ $  ]! !

