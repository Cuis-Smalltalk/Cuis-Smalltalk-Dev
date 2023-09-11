'From Cuis 6.0 [latest update: #6021] on 11 September 2023 at 3:46:21 pm'!

!ByteArray class methodsFor: 'services' stamp: 'jmv 9/11/2023 13:16:10'!
findFirstInBytes: aByteArrayOrByteString inSet: inclusionMapByteArray startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>

	inclusionMapByteArray size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aByteArrayOrByteString size.
	[ i <= stringSize and: [ (inclusionMapByteArray at: (aByteArrayOrByteString basicAt: i) +1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !


!String methodsFor: 'accessing' stamp: 'jmv 9/11/2023 13:16:35'!
indexOfAnyOf: aCharacterSet  startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start"

	| answer |
	answer := ByteArray findFirstInBytes: self inSet: aCharacterSet byteArrayMap startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]! !


!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 13:20:22'!
isAscii: aStringOrByteArray
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #nonAscii"

	| nonAsciiBytesMap |
	nonAsciiBytesMap := `(ByteArray new: 128), ((ByteArray new: 128) + 1)`.
	^ (ByteArray
		findFirstInBytes: aStringOrByteArray
		inSet: nonAsciiBytesMap
		startingAt: 1)
			= 0.! !


!ByteArray methodsFor: 'accessing' stamp: 'jmv 9/11/2023 13:16:58'!
indexOfAnyOf: aCharacterSet  startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start"

	| answer |
	answer := ByteArray findFirstInBytes: self inSet: aCharacterSet byteArrayMap startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]! !

!methodRemoval: String class #findFirstInString:inSet:startingAt: stamp: 'jmv 9/11/2023 15:45:56'!
String class removeSelector: #findFirstInString:inSet:startingAt:!
