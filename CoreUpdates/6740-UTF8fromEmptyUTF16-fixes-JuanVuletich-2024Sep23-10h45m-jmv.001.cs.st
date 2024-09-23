'From Cuis7.1 [latest update: #6739] on 23 September 2024 at 11:00:30 am'!

!UnicodeString class methodsFor: 'services' stamp: 'jmv 9/23/2024 10:45:40'!
utf8FromUtf16Bytes: utf16ByteArray bigEndianHint: bigEndianHintOrNil
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present to detect endianess, or bigEndianHintOrNil ifnotNil.
	If no BOM and bigEndianHintOrNil is nil, assume little endian."

	| isBigEndian startIndex |
	utf16ByteArray size = 0 ifTrue: [
		^#[] ].
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

!UnicodeString class methodsFor: 'services' stamp: 'jmv 9/23/2024 10:47:06'!
utf8FromUtf16Words: utf16WordArray
	"Convert the given string from UTF-16 to UTF-8.
	Skip BOM if present."

	| startIndex |
	utf16WordArray size = 0 ifTrue: [
		^#[] ].
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

