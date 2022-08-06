'From Cuis 6.0 [latest update: #5438] on 6 August 2022 at 10:39:09 am'!

!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 8/6/2022 10:38:46'!
ascii
	"return a set containing just all the 7 bit ASCII Characters"

	| set |
	set _ self new.
	0 to: 27 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	"Cuis Characters use 28, 29, 30, 31 for arrows.
	This behavior sould be removed when Cuis files are converted to use only UTF-8.
	See also #nextUtf8BytesAndCodePointInto:into:, #nonAscii and #isAscii:"
	32 to: 127 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	^set! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 8/6/2022 10:38:42'!
nonAscii
	"return a set containing all Characters that are not 7 bit ASCII"

	| set |
	set _ self new.
	"Cuis Characters use these for arrows.
	This behavior sould be removed when Cuis files are converted to use only UTF-8.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #isAscii:"
	28 to: 31 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	128 to: 255 do: [ :ascii |
		set add: (Character numericValue: ascii) ].
	^set! !


!String class methodsFor: 'services' stamp: 'jmv 8/6/2022 10:38:38'!
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


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 8/6/2022 10:38:36'!
nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock
	"Find the next UTF-8 CodePoint from this file, assuming next byte is ISO 8859-15 if invalid UTF-8,
	or nil if at the end of the file."

	| bytesConsumed previousPosition |
	previousPosition _ self position.
	bytesConsumed _ 0.
	self nextByte ifNotNil: [ :byte1 |
		"Cuis 8 bit Characters use a nonstandard meaning for codes 28, 29, 30, 31.
		#utf8BytesAndCodePointAt:in:into:into: will treat any non-valid byte sequence as Cuis Characters.
		But this doesn't include codes 28, 29, 30, 31 that are valid ASCII bytes (and therefore, valid UTF-8).
		Consider them here, as this is needed to process legacy Cuis source code.
		This behavior sould be removed when Cuis files are converted to use only UTF-8.
		See also #ascii, #nonAscii and #isAscii:"
		(byte1 between: 28 and: 31)
			ifTrue: [
				Character
					utf8BytesAndCodePointFor: 226
					byte2: 134
					byte3: (byte1//2) + (byte1\\2*2) + 130
					byte4: nil
					into: bytesBlock
					into: codePointBlock.
				bytesConsumed _ 1 ]
			ifFalse: [
				bytesConsumed _ Character
					utf8BytesAndCodePointFor: byte1
					byte2: self nextByte
					byte3: self nextByte
					byte4: self nextByte
					into: bytesBlock
					into: codePointBlock ]].
	self position: previousPosition + bytesConsumed.! !

