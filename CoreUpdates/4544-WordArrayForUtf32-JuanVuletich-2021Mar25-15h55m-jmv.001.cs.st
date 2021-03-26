'From Cuis 5.0 [latest update: #4543] on 25 March 2021 at 4:34:25 pm'!

!String class methodsFor: 'instance creation' stamp: 'jmv 3/25/2021 16:33:56'!
utf32FromUtf8: aByteArray
	"Convert the given string from UTF-8 to UTF-32"

	^WordArray streamContents: [ :strm | | bytes codePoint |
		bytes _ aByteArray readStream.
		[ bytes atEnd ] whileFalse: [
			codePoint _ (Character nextUnicodeCodePointFromUtf8: bytes).
			codePoint ifNotNil: [
				strm nextPut: codePoint ]]]! !

!String class methodsFor: 'instance creation' stamp: 'jmv 3/25/2021 16:33:31'!
utf8FromUtf32: aWordArray
	"Convert the given string from UTF-8 to UTF-32"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !

