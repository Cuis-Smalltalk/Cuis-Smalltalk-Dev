'From Cuis 6.0 [latest update: #5261] on 11 June 2022 at 3:34:09 pm'!

!Utf8String class methodsFor: 'services' stamp: 'jmv 6/11/2022 15:33:28'!
utf8FromUtf16: utf16ByteArray
	"Convert the given string from UTF-16 to UTF-8.
	Use BOM if present."

	| codePoint isBigEndian startIndex |
	isBigEndian _ false. "best guess"
	startIndex _ 1.
	(utf16ByteArray unsignedShortAt: 1 bigEndian: true) = 16rFEFF ifTrue: [
		isBigEndian _ true.
		startIndex _ 3 ].
	(utf16ByteArray unsignedShortAt: 1 bigEndian: false) = 16rFEFF  ifTrue: [
		isBigEndian _ false.
		startIndex _ 3 ].
	^ByteArray streamContents: [ :strm |
		startIndex to: utf16ByteArray size by: 2 do: [ :index |
			codePoint _ utf16ByteArray unsignedShortAt: index bigEndian: isBigEndian.
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]].! !


!Utf8String class methodsFor: 'services' stamp: 'jmv 6/11/2022 15:32:35'!
utf8FromUtf32: aWordArray
	"Convert the given string from UTF-32 to UTF-8"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !

