'From Cuis7.5 [latest update: #7581] on 17 September 2025 at 9:48:14 am'!

!DataStream methodsFor: 'reading' library: #MinimalKernel06ObjectSerialization stamp: 'jmv 9/17/2025 09:41:30'!
readInteger
    "PRIVATE -- Read the contents of a SmallInteger.
	See #writeInteger:"

	| highBits lowBits |
	highBits := byteStream nextInt32BigEndian: true.	"signed!!!!!!"
	lowBits := byteStream nextUint32BigEndian: true.	"unsigned!!!!!!"
	^(highBits bitShift: 32) + lowBits.! !


!SmallInteger class methodsFor: 'constants' library: #MinimalKernel01 stamp: 'jmv 9/17/2025 09:47:37'!
minVal
	"Answer the minimum value for a SmallInteger."
	minVal ifNil: [
		"Don't create any LargeInteger!!"
		minVal := Smalltalk wordSize = 8
			ifTrue: [((1 bitShift: 59) negated bitShift: 1)]
			ifFalse: [((1 bitShift: 29) negated bitShift: 1)]].
	^minVal.! !

