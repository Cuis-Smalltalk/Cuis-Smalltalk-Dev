'From Cuis7.1 [latest update: #6494] on 5 July 2024 at 12:37:02 pm'!

!UnicodeString class methodsFor: 'services' stamp: 'jmv 7/5/2024 12:33:23'!
utf16WordsFromUtf8: utf8ByteArray  includeBOM: wantsBOM
	"Convert the given string from UTF-8 to UTF-16.
	Include BOM if requested.
	https://en.wikipedia.org/wiki/UTF-16
	UnicodeString codePointsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162].
	(UnicodeString utf16WordsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162]
		includeBOM: false) asArray collect: [ :w | w hex ].
	(UnicodeString utf16WordsFromUtf8: #[36 226 130 172 240 144 144 183 240 164 173 162]
		includeBOM: true) asArray collect: [ :w | w hex ].
	"

	^DoubleByteArray streamContents: [ :strm | | s byteIndex n |
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
			byteIndex := byteIndex + n ]].! !

!classRemoval: #ShortWordArray stamp: 'jmv 7/5/2024 12:33:40'!
Smalltalk removeClassNamed: #ShortWordArray!

