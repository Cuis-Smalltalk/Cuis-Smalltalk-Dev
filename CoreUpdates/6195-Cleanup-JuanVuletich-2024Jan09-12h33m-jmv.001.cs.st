'From Cuis6.3 [latest update: #6187] on 9 January 2024 at 12:36:19 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:26:50'!
evaluate: aBlock withUtf16WordsOf: aCodePoint
	"See senders for typical usage"

	| highSurrogate lowSurrogate |
	(aCodePoint between: 16rD800 and: 16rDFFF) ifTrue: [
		self error: 'invalid CodePoint' ].

	aCodePoint <= 16rFFFF ifTrue: [
		^aBlock value: aCodePoint ].

	highSurrogate := (aCodePoint - 16r10000) >> 10 + 16rD800.
	lowSurrogate := ((aCodePoint - 16r10000) bitAnd: 1023) + 16rDC00.
	aBlock value: highSurrogate.
	aBlock value: lowSurrogate.! !

!Character class methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:27:12'!
evaluate: aBlock withUtf8BytesOf: aCodePoint
	"See senders for typical usage"

	| mask nBytes shift |
	aCodePoint < 128 ifTrue: [
		^aBlock value: aCodePoint ].
	nBytes := aCodePoint highBit + 3 // 5.
	mask := #(128 192 224 240 248 252 254 255) at: nBytes.
	shift := nBytes - 1 * -6.
	aBlock value: (aCodePoint bitShift: shift) + mask.
	2 to: nBytes do: [ :i | 
		shift := shift + 6.
		aBlock value: ((aCodePoint bitShift: shift) bitAnd: 63) + 128 ]! !

!Character class methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:30:39'!
evaluateOnce: aBlock withUtf8BytesOf: aCodePoint
	"Similar to #evaluate:withUtf8BytesOf:, but aBlock is evaluated just once, and must have 4 parameters."

	| mask nBytes shift byte1 byte2 byte3 byte4 |
	aCodePoint < 128 ifTrue: [
		^aBlock value: aCodePoint value: nil value: nil value: nil ].
	nBytes := aCodePoint highBit + 3 // 5.
	mask := #(128 192 224 240 248 252 254 255) at: nBytes.
	shift := nBytes - 1 * -6.
	byte1 := (aCodePoint bitShift: shift) + mask.
	nBytes >= 2 ifTrue: [
		shift := shift + 6.
		byte2 :=  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
		nBytes >= 3 ifTrue: [
			shift := shift + 6.
			byte3 :=  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
			nBytes = 4 ifTrue: [
				shift := shift + 6.
				byte4 :=  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
				].
			].
		].
		^aBlock value: byte1 value: byte2 value: byte3 value: byte4.! !

!Character class methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:29:28'!
utf8BytesOf: aCodePoint

	^ ByteArray streamContents: [ :strm |
		Character
			evaluate: [ :byte |
				strm nextPut: byte ]
			withUtf8BytesOf: aCodePoint ]! !


!Character class methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:28:42'!
utf8BytesAndCodePointForIso8859s1: byte into: bytesBlock into: codePointBlock
	"Latin-1"
	| codePoint |
	codePoint := byte.
	bytesBlock ifNotNil: [
		Character
			evaluateOnce: bytesBlock
			withUtf8BytesOf: codePoint ].
	codePointBlock ifNotNil: [
		codePointBlock value: codePoint ].
	^1! !


!String methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:27:25'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray with a valid UTF-8 sequence."

	| inStream nextChar codePoint |
	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		inStream := self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar := inStream next.
			codePoint := nextChar codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: codePoint ]]! !

!String methodsFor: 'converting' stamp: 'jmv 1/9/2024 12:27:29'!
asUtf8BytesOrByteString
	"Similar to #asUtf8Bytes.
	May answer an instance of String as an optimization.
	See senders"

	| inStream nextChar codePoint |
	"Fast lane."
	self isAscii ifTrue: [ ^self ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		inStream := self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar := inStream next.
			codePoint := nextChar codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOf: codePoint ]]! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:29:33'!
at: codePointIndex put: aCharacter
	"Store argument at position codePointIndex "

	| byteIndex currentByteCount argumentBytes newBytes |

	self boundsCheck: codePointIndex.
	byteIndex := self byteIndexAt: codePointIndex.
	currentByteCount := self encodedByteCountAt: byteIndex.

	"Fastest path if both old and new are ASCII"
	currentByteCount = 1 ifTrue: [
		aCharacter asciiValue ifNotNil: [ :newAscii |
			bytes at: 	byteIndex put: newAscii.
			^ aCharacter ]].

	argumentBytes := Character utf8BytesOf: aCharacter codePoint.

	"Avoid creating new bytes object if both old and new require same number of bytes"
	argumentBytes size = currentByteCount ifTrue: [
		bytes replaceFrom: byteIndex to: byteIndex + currentByteCount - 1 with: argumentBytes startingAt: 1.
		strideIndex := nil.
		hash := nil.
		^ aCharacter ].

	"More expensive: needs a new bytes object of different size"
	newBytes := ByteArray new: bytes size - currentByteCount + argumentBytes size.
	newBytes
		replaceFrom: 1 to: byteIndex-1 with: bytes startingAt: 1;
		replaceFrom: byteIndex to: byteIndex + argumentBytes size - 1 with: argumentBytes startingAt: 1;
		replaceFrom: byteIndex + argumentBytes size to: newBytes size with: bytes startingAt: byteIndex + currentByteCount.

	self privateBytes: newBytes codePointCount: count.
	^ aCharacter.! !

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:29:40'!
indexOf: aCharacter startingAt: start ifAbsent: aBlock
	| codePointBytes startByteIndex foundByteIndex answer |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	codePointBytes := Character utf8BytesOf: aCharacter codePoint.
	startByteIndex := self byteIndexAt: start.
	foundByteIndex := String findString: codePointBytes in: bytes startingAt: startByteIndex.
	answer := self codePointIndexAt: foundByteIndex.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 1/9/2024 12:29:36'!
copyWith: aCharacter

	^UnicodeString
		fromUtf8Bytes: bytes, (Character utf8BytesOf: aCharacter codePoint)
		codePointCount: self size + 1.! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 12:29:43'!
with: aCharacter 
	"Answer a new instance of me, containing utf bytes for aCharacter.
	UnicodeString with: $a
	UnicodeString with: $á
	UnicodeString with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOf: aCharacter codePoint)
		codePointCount: 1.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 12:29:48'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter and otherCharacter.
	UnicodeString with: $a
	UnicodeString with: $á with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOf: aCharacter codePoint),
			(Character utf8BytesOf: otherCharacter codePoint)
		codePointCount: 2.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 12:29:54'!
with: aCharacter with: otherCharacter with: thirdCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter, otherCharacter and thirdCharacter.
	UnicodeString with: $a
	UnicodeString with: $á with: Character euro
	UnicodeString with: $á with: Character euro with: $¿
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOf: aCharacter codePoint),
			(Character utf8BytesOf: otherCharacter codePoint),
			(Character utf8BytesOf: thirdCharacter codePoint)
		codePointCount: 3.! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 1/9/2024 12:26:57'!
utf16BytesFromUtf8: utf8ByteArray bigEndian: bigEndian includeBOM: wantsBOM
	"Convert the given string from UTF-8 to UTF-16.
	Honor requested endianess. Include BOM if requested."

	^ByteArray streamContents: [ :strm | | s byteIndex n |
		wantsBOM ifTrue: [
			strm nextUint16Put: 16rFEFF bigEndian: bigEndian ].
		s := utf8ByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: utf8ByteArray
				into: nil
				into: [ :codePoint |
					Character
						evaluate: [ :word | strm nextUint16Put: word bigEndian: bigEndian ]
						withUtf16WordsOf: codePoint  ].
			byteIndex := byteIndex + n ]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 1/9/2024 12:27:01'!
utf16WordsFromUtf8: utf8ByteArray  includeBOM: wantsBOM
	"Convert the given string from UTF-8 to UTF-16.
	Include BOM if requested.
	https://en.wikipedia.org/wiki/UTF-16
	UnicodeString codePointsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162].
	(UnicodeString utf16WordsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162]
		includeBOM: false) asArray collect: [ :w | w hex ].
	(UnicodeString utf16WordsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162]
		includeBOM: true) asArray collect: [ :w | w hex ].
	Note: Answer is a ShortArray, which can only have even size. Add a trailing zero if needed."

	^ShortWordArray streamContents: [ :strm | | s byteIndex n |
		wantsBOM ifTrue: [
			strm nextPut: 16rFEFF ].
		s := utf8ByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: utf8ByteArray
				into: nil
				into: [ :codePoint |
					Character
						evaluate: [ :word | strm nextPut: word ]
						withUtf16WordsOf: codePoint ].
			byteIndex := byteIndex + n ].
		strm position odd ifTrue: [ strm nextPut: 0 ]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 1/9/2024 12:27:34'!
utf8FromCodePoints: aWordArray
	"Convert the given string from UTF-32 to UTF-8"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOf: codePoint ]]! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 1/9/2024 12:28:13'!
utf8FromUtf16Bytes: utf16ByteArray bigEndianHint: bigEndianHintOrNil
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present to detect endianess, or bigEndianHintOrNil ifnotNil.
	If no BOM and bigEndianHintOrNil is nil, assume little endian."

	| isBigEndian startIndex |
	isBigEndian := bigEndianHintOrNil ifNil: [false]. "best guess"
	startIndex := 1.
	(utf16ByteArray uint16At: 1 bigEndian: true) = 16rFEFF ifTrue: [
		isBigEndian := true.
		startIndex := 3 ].
	(utf16ByteArray uint16At: 1 bigEndian: false) = 16rFEFF  ifTrue: [
		isBigEndian := false.
		startIndex := 3 ].
	^ByteArray streamContents: [ :strm | | s byteIndex word |
		s := utf16ByteArray size.
		byteIndex := startIndex.
		[ byteIndex <= s ] whileTrue: [
			word := utf16ByteArray uint16At: byteIndex bigEndian: isBigEndian.
			(word between: 16rD800 and: 16rDFFF)
				ifFalse: [
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: word.
					byteIndex := byteIndex + 2 ]
				ifTrue: [ | highSurrogatePart lowSurrogatePart codePoint |
					(word between: 16rD800 and: 16rDBFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					highSurrogatePart := word - 16rD800.
					word := utf16ByteArray uint16At: byteIndex+2 bigEndian: isBigEndian.
					(word between: 16rDC00 and: 16rDFFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					lowSurrogatePart := word - 16rDC00.
					codePoint := highSurrogatePart << 10 + lowSurrogatePart + 16r10000.
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: codePoint.
					byteIndex := byteIndex + 4 ]]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 1/9/2024 12:28:18'!
utf8FromUtf16Words: utf16WordArray
	"Convert the given string from UTF-16 to UTF-8.
	Skip BOM if present."

	| startIndex |
	startIndex := 1.
	(utf16WordArray at: 1) = 16rFEFF ifTrue: [
		startIndex := 2 ].
	^ByteArray streamContents: [ :strm | | s wordIndex word |
		s := utf16WordArray size.
		wordIndex := startIndex.
		[ wordIndex <= s ] whileTrue: [
			word := utf16WordArray at: wordIndex.
			(word between: 16rD800 and: 16rDFFF)
				ifFalse: [
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: word.
					wordIndex := wordIndex + 1 ]
				ifTrue: [ | highSurrogatePart lowSurrogatePart codePoint |
					(word between: 16rD800 and: 16rDBFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					highSurrogatePart := word - 16rD800.
					word := utf16WordArray at: wordIndex+1.
					(word between: 16rDC00 and: 16rDFFF) ifFalse: [ self error: 'Invalid UTF-16' ].
					lowSurrogatePart := word - 16rDC00.
					codePoint := highSurrogatePart << 10 + lowSurrogatePart + 16r10000.
					Character
						evaluate: [ :byte | strm nextPut: byte ]
						withUtf8BytesOf: codePoint.
					wordIndex := wordIndex + 2 ]]].! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 1/9/2024 12:27:45'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOf: aNumber! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 1/9/2024 12:27:49'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	| n |
	"Save in a temp because #nextPutByte: smashes the ivar."
	n := numberOfCodePointsWritten ifNotNil: [ numberOfCodePointsWritten + 1 ].
	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOf: aNumber.
	numberOfCodePointsWritten := n.! !


!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 1/9/2024 12:28:46'!
widthOf: aCharacterOrCodePoint

	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
			^self widthOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil ]
		withUtf8BytesOf: aCharacterOrCodePoint codePoint.! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/9/2024 12:28:01'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes leftArrowGlyph upArrowGlyph rightArrowGlyph downArrowGlyph carriageReturnGlyph leftArrowGlyphCodePoint upArrowGlyphCodePoint rightArrowGlyphCodePoint downArrowGlyphCodePoint carriageReturnGlyphCodePoint |
	unknownGlyph := glyphs first.
	nullGlyph := glyphs second.
	mapSize := characterMap size.
	glyphsByUTF8 := Array new: 256.
	leftArrowGlyphCodePoint := self class smalltalkAssignmentArrowGlyphCodePoint.
	upArrowGlyphCodePoint := self class smalltalkReturnArrowGlyphCodePoint.
	rightArrowGlyphCodePoint := 8594.
	downArrowGlyphCodePoint := 8595.
	carriageReturnGlyphCodePoint := 8617.
	
	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes := {leftArrowGlyphCodePoint. upArrowGlyphCodePoint. rightArrowGlyphCodePoint. downArrowGlyphCodePoint. carriageReturnGlyphCodePoint}
					collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs := arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	"Take arrow glyphs if present, or use fallbacks if not."
	leftArrowGlyph := upArrowGlyph := rightArrowGlyph := downArrowGlyph := carriageReturnGlyph := unknownGlyph.
	mapSize > downArrowGlyphCodePoint ifTrue: [
		leftArrowGlyph := glyphs at: (characterMap at: leftArrowGlyphCodePoint+1)+1.
		upArrowGlyph := glyphs at: (characterMap at: upArrowGlyphCodePoint+1)+1.
		rightArrowGlyph := glyphs at: (characterMap at: rightArrowGlyphCodePoint+1)+1.
		downArrowGlyph := glyphs at: (characterMap at: downArrowGlyphCodePoint+1)+1 ].
	leftArrowGlyph == unknownGlyph ifTrue: [ leftArrowGlyph := FallbackArrowGlyphs at: 1 ].
	upArrowGlyph == unknownGlyph ifTrue: [ upArrowGlyph := FallbackArrowGlyphs at: 2 ].
	rightArrowGlyph == unknownGlyph ifTrue: [ rightArrowGlyph := FallbackArrowGlyphs at: 3 ].
	downArrowGlyph == unknownGlyph ifTrue: [ downArrowGlyph := FallbackArrowGlyphs at: 4 ].
	mapSize > carriageReturnGlyphCodePoint ifTrue: [
		carriageReturnGlyph := glyphs at: (characterMap at: carriageReturnGlyphCodePoint+1)+1 ].
	carriageReturnGlyph == unknownGlyph ifTrue: [ carriageReturnGlyph := FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint := codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
		g := glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g := glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte := byte.
				]
			withUtf8BytesOf: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ Character zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOf: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		s codePoint < 256 ifTrue: [
			glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ]].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph.​ upArrowGlyph.​ rightArrowGlyph.​ downArrowGlyph. carriageReturnGlyph}! !

!methodRemoval: Character class #evaluateOnce:withUtf8BytesOfUnicodeCodePoint: stamp: 'jmv 1/9/2024 12:33:58'!
Character class removeSelector: #evaluateOnce:withUtf8BytesOfUnicodeCodePoint:!
!methodRemoval: Character class #evaluate:withUtf8BytesOfUnicodeCodePoint: stamp: 'jmv 1/9/2024 12:33:58'!
Character class removeSelector: #evaluate:withUtf8BytesOfUnicodeCodePoint:!
!methodRemoval: Character class #evaluate:withUtf16WordsOfUnicodeCodePoint: stamp: 'jmv 1/9/2024 12:33:58'!
Character class removeSelector: #evaluate:withUtf16WordsOfUnicodeCodePoint:!
!methodRemoval: Character class #utf8BytesOfUnicodeCodePoint: stamp: 'jmv 1/9/2024 12:33:58'!
Character class removeSelector: #utf8BytesOfUnicodeCodePoint:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ChangeSet zapAllChangeSets.
Smalltalk garbageCollect.!

