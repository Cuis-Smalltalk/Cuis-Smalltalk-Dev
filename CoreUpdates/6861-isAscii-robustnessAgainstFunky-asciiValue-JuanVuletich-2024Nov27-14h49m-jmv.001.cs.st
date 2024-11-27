'From Cuis7.1 [latest update: #6860] on 27 November 2024 at 3:00:40 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 11/27/2024 14:54:30'!
isAscii
	"Am I a 7-bit ASCII Character?"

	^ self codePoint < 128! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 11/27/2024 14:55:18'!
at: codePointIndex put: aCharacter
	"Store argument at position codePointIndex "

	| byteIndex currentByteCount argumentBytes newBytes |

	self boundsCheck: codePointIndex.
	byteIndex := self byteIndexAt: codePointIndex.
	currentByteCount := self encodedByteCountAt: byteIndex.

	"Fastest path if both old and new are ASCII"
	currentByteCount = 1 ifTrue: [
		aCharacter isAscii ifTrue: [
			bytes at: byteIndex put: aCharacter asciiValue.
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


!Scanner class methodsFor: 'testing' stamp: 'jmv 11/27/2024 14:56:06'!
typeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint isAscii ifTrue: [
		^aTypeTable at: aCharacterOrCodePoint asciiValue ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint = $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !


!Character methodsFor: 'accessing' stamp: 'jmv 11/27/2024 14:54:06'!
asciiValue
	"Answer the value of the receiver that represents its ASCII encoding.
	This is the same as the Unicode Code Point for the first 128 characters.
	Answer nil if out of range, i.e. we are not part of the ASCII Character set.

	Note: Senders, please assume this method could be redefined to answer
	the #codePoint even if not ASCII. Please protect yourselves from this."

	^self isAscii ifTrue: [ self codePoint ].! !

