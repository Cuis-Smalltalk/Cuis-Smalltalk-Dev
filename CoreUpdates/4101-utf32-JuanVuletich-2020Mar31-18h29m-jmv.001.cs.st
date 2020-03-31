'From Cuis 5.0 [latest update: #4100] on 31 March 2020 at 6:32:04 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 3/31/2020 18:30:21'!
evaluateOnce: aBlock withUtf8BytesOfUnicodeCodePoint: aCodePoint
	"Similar to #evaluate:withUtf8BytesOfUnicodeCodePoint:, but aBlock is evaluated just once, and must have 4 parameters."

	| mask nBytes shift byte1 byte2 byte3 byte4 |
	aCodePoint < 128 ifTrue: [
		^aBlock value: aCodePoint value: nil value: nil value: nil ].
	nBytes _ aCodePoint highBit + 3 // 5.
	mask _ #(128 192 224 240 248 252 254 255) at: nBytes.
	shift _ nBytes - 1 * -6.
	byte1 _ (aCodePoint bitShift: shift) + mask.
	nBytes >= 2 ifTrue: [
		shift _ shift + 6.
		byte2 _  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
		nBytes >= 3 ifTrue: [
			shift _ shift + 6.
			byte3 _  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
			nBytes = 4 ifTrue: [
				shift _ shift + 6.
				byte4 _  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
				].
			].
		].
		^aBlock value: byte1 value: byte2 value: byte3 value: byte4.! !


!String class methodsFor: 'instance creation' stamp: 'jmv 3/31/2020 16:22:54'!
utf32FromUtf8: aByteArray
	"Convert the given string from UTF-8 to UTF-32"

	^IntegerArray streamContents: [ :strm | | bytes codePoint |
		bytes _ aByteArray readStream.
		[ bytes atEnd ] whileFalse: [
			codePoint _ (Character nextUnicodeCodePointFromUtf8: bytes).
			codePoint ifNotNil: [
				strm nextPut: codePoint ]]]! !

!String class methodsFor: 'instance creation' stamp: 'jmv 3/31/2020 16:25:01'!
utf8FromUtf32: anIntegerArray
	"Convert the given string from UTF-8 to UTF-32"

	^ByteArray streamContents: [ :strm |
		anIntegerArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !

