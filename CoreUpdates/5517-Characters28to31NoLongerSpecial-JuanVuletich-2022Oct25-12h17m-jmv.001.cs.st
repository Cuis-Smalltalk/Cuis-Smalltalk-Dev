'From Cuis 6.0 [latest update: #5506] on 25 October 2022 at 12:28:36 pm'!

!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 11:11:34'!
ascii
	"return a set containing just all the 7 bit ASCII Characters"

	| set |
	set _ self new.
	0 to: 127 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	^set! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 10/25/2022 12:26:37'!
nonAscii
	"return a set containing all Characters that are not 7 bit ASCII"

	| set |
	set _ self new.
	128 to: 255 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	^set! !


!String class methodsFor: 'services' stamp: 'jmv 10/25/2022 12:27:39'!
isAscii: aStringOrByteArray
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8.
	Note: Consider codes 28, 29, 30, 31 as non ascii, as they are used in Cuis for Unicode arrows.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #nonAscii"

	^ (String
		findFirstInString: aStringOrByteArray
		inSet: `(CharacterSet nonAscii) byteArrayMap`
		startingAt: 1)
			= 0! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/25/2022 12:25:59'!
nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock
	"Find the next UTF-8 CodePoint from this file, assuming next byte is ISO 8859-15 if invalid UTF-8,
	or nil if at the end of the file."

	| bytesConsumed previousPosition |
	previousPosition _ self position.
	bytesConsumed _ 0.
	self nextByte ifNotNil: [ :byte1 |
		bytesConsumed _ Character
			utf8BytesAndCodePointFor: byte1
			byte2: self nextByte
			byte3: self nextByte
			byte4: self nextByte
			into: bytesBlock
			into: codePointBlock ].
	self position: previousPosition + bytesConsumed.! !

