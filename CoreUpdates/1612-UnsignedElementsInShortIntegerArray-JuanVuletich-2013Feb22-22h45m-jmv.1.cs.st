'From Cuis 4.1 of 12 December 2012 [latest update: #1611] on 23 February 2013 at 12:37:13 am'!

!IntegerArray commentStamp: '<historical>' prior: 0!
IntegerArrays store 32bit signed Integer values, with values between -16r80000000 and 16r7FFFFFFF.
Negative values are stored as 2's complement.!


!ShortIntegerArray commentStamp: 'jmv 2/23/2013 00:36' prior: 0!
ShortIntegerArray is an array for efficiently representing signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.
Additionaly, using #at:putUnsigned: and #at:putSigned:, it supports unsigned 16 bits integers, between 0 and 16rFFFF.!


!WordArray commentStamp: '<historical>' prior: 0!
WordArrays store 32-bit unsigned Integer values, between 0 and 16rFFFFFFFF.
!


!Integer methodsFor: 'bit manipulation' stamp: 'jmv 2/22/2013 22:48'!
bitInvert16
	"Answer the 16-bit complement of the receiver."

	^ self bitXor: 16rFFFF! !


!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/22/2013 22:50'!
at: index putSigned: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	^self at: index put: value! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/23/2013 00:30'!
at: index putUnsigned: value
	"Store the given 16-bit unsigned integer at the given index in the receiver."

	| primValue |
	value < 0 ifTrue: [ self errorImproperStore ].
	primValue _ value > 16r7FFF
			ifTrue: [ (value bitInvert16 + 1) negated ]
			ifFalse: [ value ].
	self at: index put: primValue.
	^value! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/22/2013 22:50'!
signedAt: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	^self at: index! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/22/2013 22:54'!
unsignedAt: index
	"Return the 16-bit unsigned integer value at the given index of the receiver."

	| primValue |
	primValue _ self at: index.
	^primValue < 0
		ifTrue: [ (primValue + 1) negated bitInvert16 ]
		ifFalse: [ primValue ]! !


!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/22/2013 22:46'!
at: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	<primitive: 143>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber ifTrue: [^ self at: index truncated].
	self errorNonIntegerIndex.
! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/22/2013 22:46'!
at: index put: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [
			(index >= 1 and: [index <= self size])
				ifTrue: [self errorImproperStore]
				ifFalse: [self errorSubscriptBounds: index]].
	index isNumber ifTrue: [^ self at: index truncated put: value].
	self errorNonIntegerIndex.
! !

