'From Cuis 6.0 [latest update: #6070] on 15 November 2023 at 2:32:36 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 11/15/2023 11:53:29'!
evaluate: aBlock withUtf16WordsOfUnicodeCodePoint: aCodePoint
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


!UnicodeString class methodsFor: 'services' stamp: 'jmv 11/15/2023 12:06:51'!
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
						withUtf16WordsOfUnicodeCodePoint: codePoint  ].
			byteIndex := byteIndex + n ]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 11/15/2023 12:02:12'!
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
						withUtf16WordsOfUnicodeCodePoint: codePoint ].
			byteIndex := byteIndex + n ].
		strm position odd ifTrue: [ strm nextPut: 0 ]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 11/15/2023 12:57:44'!
utf8FromUtf16Bytes: utf16ByteArray
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present to detect endianess.
	If no BOM, assume little endian."

	^self utf8FromUtf16Bytes: utf16ByteArray bigEndianHint: nil! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 11/15/2023 12:55:34'!
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
						withUtf8BytesOfUnicodeCodePoint: word.
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
						withUtf8BytesOfUnicodeCodePoint: codePoint.
					byteIndex := byteIndex + 4 ]]].! !

!UnicodeString class methodsFor: 'services' stamp: 'jmv 11/15/2023 12:39:33'!
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
						withUtf8BytesOfUnicodeCodePoint: word.
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
						withUtf8BytesOfUnicodeCodePoint: codePoint.
					wordIndex := wordIndex + 2 ]]].! !

!methodRemoval: UnicodeString class #utf8FromUtf16: stamp: 'jmv 11/15/2023 14:32:04'!
UnicodeString class removeSelector: #utf8FromUtf16:!
