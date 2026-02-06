'From Cuis7.7 [latest update: #7829] on 6 February 2026 at 3:50:25 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 6/Feb/2026 15:47:03'!
typeTableAt: aCharacter
	^self class typeTable: typeTable at: aCharacter! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 6/Feb/2026 15:46:51'!
typeTable: aTypeTable at: aCharacter

	"Prebuilt tables for the ASCII range"
	aCharacter isAscii ifTrue: [
		^aTypeTable at: aCharacter asciiValue ].

	"For CodePoints not in the ASCII range"
	aCharacter isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacter isLetter ifTrue: [ ^#xLetter ].
	aCharacter isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacter == $­ ifTrue: [ ^#xBinary ].
	aCharacter isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !

!Scanner class methodsFor: 'testing' stamp: 'jmv 6/Feb/2026 15:47:10'!
typeTableAt: aCharacter
	TypeTable ifNil: [
		self initTypeTable ].
	^self typeTable: TypeTable at: aCharacter! !


!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 6/Feb/2026 15:47:51'!
widthOf: aCharacter

	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
			^self widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil ]
		withUtf8BytesOf: aCharacter codePoint.! !

