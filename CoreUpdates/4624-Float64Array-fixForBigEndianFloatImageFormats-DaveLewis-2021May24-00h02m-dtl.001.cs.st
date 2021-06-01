'From Cuis 5.0 [latest update: #4607] on 24 May 2021 at 2:20:35 pm'!

!Float class methodsFor: 'testing' stamp: 'dtl 5/24/2021 14:17:21'!
nativeWordOrdering
	"True if this image stores float objects internally in native word order.
	If false, double word floats are stored in big-endian order regardless
	of the machine native word order."
	
	^ Smalltalk imageFormatVersion anyMask: 1
! !


!Float64Array methodsFor: 'accessing' stamp: 'dtl 5/24/2021 14:17:31'!
floatAt: index put: aNumber
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as BoxedFloat64. I.e. a BoxedFloat64 and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:put:"

	"This breaks with SmallFloat64"
	"self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat asFloat startingAt: 1."

	"Float >>basicAt: acts as if Floats were stored in big endian format. Our instances are in platform endianess."
	| aFloat |
	aFloat _ aNumber asFloat.
	(Smalltalk isLittleEndian and: [Float nativeWordOrdering])
		ifTrue: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 2).
			self basicAt: index * 2 put: (aFloat basicAt: 1) ]
		ifFalse: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 1).
			self basicAt: index * 2 put: (aFloat basicAt: 2) ].
	^aFloat! !


!Float class reorganize!
('instance creation' basicNew basicNew: fromIEEE32Bit: new new: readFrom: signBit:mantissaBits:exponentBits: signPart:mantissaPart:exponentPart:)
('constants' denormalized e emax emin epsilon fmax fmin fminDenormalized fminNormalized halfPi infinity ln10 ln2 maxExactInteger nan negativeInfinity negativeZero one pi precision radix twoPi zero)
('class initialization' initClassCachedState initialize releaseClassCachedState)
('testing' nativeWordOrdering)
!

