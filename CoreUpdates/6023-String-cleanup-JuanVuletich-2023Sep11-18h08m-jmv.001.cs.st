'From Cuis 6.0 [latest update: #6022] on 11 September 2023 at 6:09:44 pm'!

!ByteArray class methodsFor: 'services' stamp: 'jmv 9/11/2023 17:57:34'!
indexOfByte: anInteger inBytes: aByteArrayOrByteString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	start > 0 ifFalse: [ ^ 0 ].
	stringSize := aByteArrayOrByteString size.
	start to: stringSize do: [:pos |
		(aByteArrayOrByteString basicAt: pos) = anInteger ifTrue: [^ pos]].
	^ 0
! !


!CharacterSet methodsFor: 'testing' stamp: 'jmv 9/11/2023 17:57:51'!
isAscii
	"Answer true if we only contain ASCII characters
	(CharacterSet newFrom: { $a. $b }) isAscii.
	(CharacterSet newFrom: { $á. $ê }) isAscii.
	"

	^(ByteArray indexOfByte: 1 inBytes: map startingAt: 128) = 0! !


!String methodsFor: 'accessing' stamp: 'jmv 9/11/2023 17:58:09'!
indexOf: aCharacter startingAt: start ifAbsent: aBlock
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	ans := ByteArray indexOfByte: aCharacter iso8859s15Code inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !


!ByteArray methodsFor: 'accessing' stamp: 'jmv 9/11/2023 17:58:16'!
indexOf: anInteger startingAt: start ifAbsent: aBlock

	| ans |
	(anInteger isInteger and: [
		anInteger >= 0 and: [
		anInteger <= 255 ] ]) ifFalse: [ ^0 ].
	ans := ByteArray indexOfByte: anInteger inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!methodRemoval: String class #indexOfByte:inString:startingAt: stamp: 'jmv 9/11/2023 18:09:09'!
String class removeSelector: #indexOfByte:inString:startingAt:!
