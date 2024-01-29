'From Cuis6.3 [latest update: #6169] on 8 January 2024 at 9:39:36 am'!

!Character class methodsFor: 'converting' stamp: 'jmv 1/7/2024 19:23:27'!
utf8BytesAndCodePointForIso8859s1: byte into: bytesBlock into: codePointBlock
	"Latin-1"
	| codePoint |
	codePoint := byte.
	bytesBlock ifNotNil: [
		Character
			evaluateOnce: bytesBlock
			withUtf8BytesOfUnicodeCodePoint: codePoint ].
	codePointBlock ifNotNil: [
		codePointBlock value: codePoint ].
	^1! !


!Character methodsFor: 'accessing' stamp: 'jmv 1/7/2024 19:09:14'!
codePoint
	"Unicode codePoint is blah blah blah"
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $€ codePoint hex = '16r20AC'.
	"
	<primitive: 171>
	^self primitiveFailed! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 08:44:44'!
codePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-1 character
	
	self assert: (Character codePoint: 16r41) = $A.
	self assert: (Character codePoint: 16r20AC) = $€.
	"
	^ codePoint < 256 ifTrue: [
		Character numericValue: codePoint ].! !

!Character class methodsFor: 'converting' stamp: 'jmv 1/7/2024 19:30:48'!
utf8BytesAndCodePointFor: byte1 byte2: byte2 byte3: byte3 byte4: byte4 into: bytesBlock into: codePointBlock
	"Find next UTF-8 encoded code point in argument bytes.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, consider the first byte an ISO-8859-1 Character, and answer 1.
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| codePoint |
	byte1 < 128 ifTrue: [	"single byte"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: nil value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: byte1 ].
		^ 1 ].
	
	"At least 2 bytes"
	(byte2 notNil and: [ (byte2 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1."
		^ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ].
	(byte1 bitAnd: 16rE0) = 16rC0  ifTrue: [ "two bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePoint := ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63).
			codePointBlock value: codePoint ].
		^2 ].
	
	"At least 3 bytes"
	(byte3 notNil and: [ (byte3 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1."
		^ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ].
	(byte1 bitAnd: 16rF0) = 16rE0  ifTrue: [ "three bytes"
		codePoint :=  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		codePoint = 16rFEFF ifTrue: [ "BOM. Ignore it. Skip ti."
			^3 ].
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: codePoint ].
		^3 ].

	"4 bytes"
	(byte4 notNil and: [ (byte4 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1."
		^ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ].
	(byte1 bitAnd: 16rF8) = 16rF0  ifTrue: [  "four bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: byte4 ].
		codePointBlock ifNotNil: [
			codePoint := ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12)
					+ ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63).
			codePointBlock value: codePoint ].
		^ 4].

	"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1."
	^ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock.! !


!UnicodeCodePoint methodsFor: 'accessing' stamp: 'jmv 1/8/2024 09:20:21'!
numericValue
self flag: #unicodeNeedsImprovement.
"To be removed. make it the Latin-1 codePoint for the time being.
This doesn't make much sense."
	value < 256 ifTrue: [ ^value ].
	^255! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 1/7/2024 19:41:52'!
< aCodePointOrCharacter
"pensar"
	^self codePoint < aCodePointOrCharacter codePoint! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 1/8/2024 09:19:22'!
hash
	"Hash is reimplemented because = is implemented."

	^value hash! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:04:18'!
asLowercase
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (Character numericValue: value) asLowercase codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:13:07'!
asUnaccented
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (Character numericValue: value) asUnaccented codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:04:26'!
asUppercase
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (Character numericValue: value) asUppercase codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:11:20'!
isAccented
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^(Character numericValue: value) isAccented ].
	^false! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:11:14'!
isUnaccented
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^(Character numericValue: value) isUnaccented ].
	^false! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:11:46'!
isValidInFilenames
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^(Character numericValue: value) isValidInFilenames ].
	^true! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:11:07'!
isVowel
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^ (Character numericValue: value) isVowel ].
	^false! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/8/2024 09:27:04'!
withDiacriticalMark: anUnicodeCodePoint
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (
			(Character numericValue: value) withDiacriticalMark: anUnicodeCodePoint)
				codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 1/8/2024 09:08:30'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	value <= 32 ifTrue: [
		aStream nextPut: $".
		aStream nextPutAll: self hex.
		aStream nextPut: $" ].! !


!Date methodsFor: 'printing' stamp: 'jmv 1/7/2024 19:44:37'!
printOn: aStream format: formatArray 
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in printOn: and mmddyy"
	| gregorian twoDigits element monthFormat |
	gregorian := self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
	twoDigits := formatArray size > 6 and: [(formatArray at: 7) > 1].
	1 to: 3 do: 
		[ :i | 
			element := formatArray at: i.
			element = 1
				ifTrue: [twoDigits
						ifTrue: [aStream
								nextPutAll: (gregorian first printString
										padded: #left
										to: 2
										with: $0)]
						ifFalse: [gregorian first printOn: aStream]].
			element = 2
				ifTrue: [monthFormat := formatArray at: 5.
					monthFormat = 1
						ifTrue: [twoDigits
								ifTrue: [aStream
										nextPutAll: (gregorian middle printString
												padded: #left
												to: 2
												with: $0)]
								ifFalse: [gregorian middle printOn: aStream]].
					monthFormat = 2
						ifTrue: [aStream
								nextPutAll: ((Month nameOfMonth: gregorian middle)
										copyFrom: 1
										to: 3)].
					monthFormat = 3
						ifTrue: [aStream
								nextPutAll: (Month nameOfMonth: gregorian middle)]].
			element = 3
				ifTrue: [(formatArray at: 6)
							= 1
						ifTrue: [gregorian last printOn: aStream]
						ifFalse: [aStream
								nextPutAll: ((gregorian last \\ 100) printString
										padded: #left
										to: 2
										with: $0)]].
			i < 3
				ifTrue: [(formatArray at: 4)
							~= 0
						ifTrue: [
							aStream nextPut: ((formatArray at: 4) isNumber
								ifTrue: [ Character numericValue: (formatArray at: 4) ]
								ifFalse: [ (formatArray at: 4) ])
							]]]
! !


!String methodsFor: 'accessing' stamp: 'jmv 1/8/2024 08:51:38'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	

			aCharacter class == UnicodeCodePoint ifTrue: [
				aCharacter codePoint > 255 ifTrue: [self error: 'String only store Latin-1 Characters'].
				^self at: index put: (Character numericValue: aCharacter codePoint) ].

			aCharacter numericValue > 255 ifTrue: [
				self error: 'String only store Latin-1 Characters'].
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']! !

!String methodsFor: 'accessing' stamp: 'jmv 1/8/2024 09:21:57'!
indexOf: aCharacter startingAt: start ifAbsent: aBlock
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	"String can only hold ASCII + Latin1"
	aCharacter codePoint >255 ifTrue: [ ^ aBlock value ].
	ans := ByteArray indexOfByte: aCharacter codePoint inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'enumerating' stamp: 'jmv 1/8/2024 08:46:40'!
collect: aBlock
	"Refer to the comment in Collection >> #collect:."
	| array resultClass |
	array := self collectInArray: aBlock.
	resultClass := String.
	array do: [ :element |
		element isCharacter ifFalse: [ ^array ].
		"String can only hold ISO-8859-1 (ASCII + Latin 1 Suplement)"
		element codePoint > 255 ifTrue: [
			resultClass := UnicodeString ]].
	^resultClass newFrom: array! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/8/2024 09:24:00'!
indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start.
	The set can only contain ASCII + Latin1 codePoints, i.e. < 256."

	"Fast lane."
	| index |
	self isAscii ifTrue: [
		^ bytes indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock ].

	"If only ASCII bytes are included in the map..."
	(ByteArray indexOfByte: 1 inBytes: aByteSetByteArray startingAt: 128) = 0 ifTrue: [
		|startBI foundBI|
		startBI := self byteIndexAt: start.
		foundBI := bytes indexOfAnyIn: aByteSetByteArray startingAt: startBI ifAbsent: aBlock.
		^ self codePointIndexAt: foundBI ].

	index := start.
	self do: [ :codePoint |
		codePoint < 256 ifTrue: [
			(aByteSetByteArray at: codePoint + 1) > 0 ifTrue: [ ^index ]].
		index := index + 1].

	^aBlock value! !


!RWBinaryOrTextStream methodsFor: 'writing' stamp: 'jmv 1/7/2024 19:45:46'!
nextPut: charOrByte

	super nextPut: 
		(charOrByte isCharacter
			ifTrue: [ charOrByte ]
			ifFalse: [ Character numericValue: charOrByte ])! !


!Encoder methodsFor: 'encoding' stamp: 'jmv 1/7/2024 19:46:50'!
asciiStringLiteral: literalObject
	"Convert UnicodeStrings to Strings if ASCII.
	Same for UnicodeCodePoint and Character.
	Recursive for Arrays"
	
	literalObject class == Array ifTrue: [
		^literalObject collect: [ :each | self asciiStringLiteral: each ]].

	"Prefer String over UnicodeString for literals, if ASCII only."
	(literalObject class == UnicodeString and: [ literalObject isAscii ]) ifTrue: [
		^ literalObject asByteString ].
	
	"Prefer Character over UnicodeCodePoint for literals, if ASCII only."
	"bueh......"
	(literalObject class == UnicodeCodePoint and: [ literalObject asciiValue notNil ]) ifTrue: [
		^ Character codePoint: literalObject codePoint ].

	^literalObject! !

!methodRemoval: UnicodeCodePoint #iso8859s15Code stamp: 'jmv 1/8/2024 09:24:37'!
UnicodeCodePoint removeSelector: #iso8859s15Code!
!methodRemoval: UnicodeCodePoint #asCharacter stamp: 'jmv 1/8/2024 09:28:50'!
UnicodeCodePoint removeSelector: #asCharacter!
!methodRemoval: Character class #unicodeCodePoints stamp: 'jmv 1/8/2024 08:57:51'!
Character class removeSelector: #unicodeCodePoints!
!methodRemoval: Character class #basicUnicodeCodePoints stamp: 'jmv 1/8/2024 08:58:47'!
Character class removeSelector: #basicUnicodeCodePoints!
!methodRemoval: Character class #basicIrregularIso8859s15Codes stamp: 'jmv 1/8/2024 08:55:58'!
Character class removeSelector: #basicIrregularIso8859s15Codes!
!methodRemoval: Character class #irregularIso8859s15Codes stamp: 'jmv 1/8/2024 08:55:45'!
Character class removeSelector: #irregularIso8859s15Codes!
!methodRemoval: Character class #utf8BytesAndCodePointForIso8859s15:into:into: stamp: 'jmv 1/8/2024 09:18:02'!
Character class removeSelector: #utf8BytesAndCodePointForIso8859s15:into:into:!
!methodRemoval: Character class #iso8859s15CodeForUnicodeCodePoint: stamp: 'jmv 1/8/2024 08:55:14'!
Character class removeSelector: #iso8859s15CodeForUnicodeCodePoint:!
!methodRemoval: Character #iso8859s15Code stamp: 'jmv 1/8/2024 09:24:37'!
Character removeSelector: #iso8859s15Code!
!methodRemoval: Character #is: stamp: 'jmv 1/8/2024 08:51:53'!
Character removeSelector: #is:!
!methodRemoval: Character #asCharacter stamp: 'jmv 1/8/2024 09:28:50'!
Character removeSelector: #asCharacter!
!methodRemoval: Integer #asCharacter stamp: 'jmv 1/8/2024 09:28:49'!
Integer removeSelector: #asCharacter!

!UnicodeCodePoint reorganize!
('accessing' asciiValue codePoint digitValue numericValue)
('testing' canBeGlobalVarInitial isAlphaNumeric isCharacter isDiacriticalMark isDigit isLetter isLineSeparator isLowercase isNullCharacter isSeparator isSymbolLike isUnicodeCodePoint isUppercase isValidInBinarySelectors isValidInIdentifiers isValidStartOfIdentifiers tokenish)
('copying' shallowCopy)
('comparing' < <= = > >= hash)
('converting' asLowercase asString asSymbol asText asUnaccented asUnicodeCodePoint asUppercase isAccented isUnaccented isValidInFilenames isVowel to: withDiacriticalMark:)
('printing' hex isLiteral printOn: storeOn:)
('private' privateValue:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!String class reorganize!
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with: with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('fileman-constants' pathSeparators)
('system' symbolClassToUse)
('stream creation' writeStream)
('services' compare:with: compare:with:collated: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: is:equalTo: isAscii: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder caseSensitiveOrder lowercasingTable tokenishTable uppercasingTable)
!

