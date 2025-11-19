'From Cuis7.5 [latest update: #7690] on 19 November 2025 at 5:36:42 pm'!

!Float32Array methodsFor: 'accessing' stamp: 'len 11/19/2025 17:24:40'!
at: index
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Float32Array methodsFor: 'accessing' stamp: 'len 11/19/2025 17:33:33'!
at: index put: value
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue: [self basicAt: index put: value asIEEE32BitWord]
		ifFalse: [self at: index put: value asFloat].
	^value! !


!Float64Array methodsFor: 'accessing' stamp: 'len 11/19/2025 17:28:37'!
at: index
	"Return the element (e.g., 64 bit Float) at the given index.
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits."
	<primitive: 238 error: ec>
	^ Float new replaceWordsFrom: 1 to: 2 with: self startingAt: index * 2 - 1! !

!Float64Array methodsFor: 'accessing' stamp: 'len 11/19/2025 17:31:18'!
at: index put: aNumber
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as BoxedFloat64. I.e. a BoxedFloat64 and a Float64Array of size 1 hold the same bits."

	"This breaks with SmallFloat64"
	"self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat asFloat startingAt: 1."

	"Float >>basicAt: acts as if Floats were stored in big endian format. Our instances are in platform endianess."
	<primitive: 239 error: ec>
	| aFloat |
	aFloat := aNumber asFloat.
	(Smalltalk isLittleEndian and: [Float nativeWordOrdering])
		ifTrue: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 2).
			self basicAt: index * 2 put: (aFloat basicAt: 1) ]
		ifFalse: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 1).
			self basicAt: index * 2 put: (aFloat basicAt: 2) ].
	^aFloat! !


!Float32PointArray methodsFor: 'accessing' stamp: 'len 11/19/2025 17:26:00'!
at: index
	"Return the element (e.g., point) at the given index"
	^(super at: index * 2 - 1) @ (super at: index * 2)! !

!Float32PointArray methodsFor: 'accessing' stamp: 'len 11/19/2025 17:32:27'!
at: index put: aPoint
	"Store the argument aPoint at the given index"
	super at: index * 2 - 1 put: aPoint x.
	super at: index * 2 put: aPoint y.
	^aPoint! !

!Float32PointArray methodsFor: 'accessing' stamp: 'len 11/19/2025 17:32:38'!
at: index putX: aNumber
	"Store the argument aPoint at the given index"
	super at: index * 2 - 1 put: aNumber! !

!Float32PointArray methodsFor: 'accessing' stamp: 'len 11/19/2025 17:32:46'!
at: index putY: aNumber
	"Store the argument aPoint at the given index"
	super at: index * 2 put: aNumber! !

!methodRemoval: Float64Array #floatAt: stamp: 'len 11/19/2025 17:33:12'!
Float64Array removeSelector: #floatAt:!
!methodRemoval: Float64Array #floatAt:put: stamp: 'len 11/19/2025 17:33:54'!
Float64Array removeSelector: #floatAt:put:!
!methodRemoval: Float32Array #floatAt: stamp: 'len 11/19/2025 17:33:11'!
Float32Array removeSelector: #floatAt:!
!methodRemoval: Float32Array #floatAt:put: stamp: 'len 11/19/2025 17:33:53'!
Float32Array removeSelector: #floatAt:put:!
!methodRemoval: FloatArray #at2: stamp: 'len 11/19/2025 17:25:10'!
FloatArray removeSelector: #at2:!
!methodRemoval: FloatArray #at2:put: stamp: 'len 11/19/2025 17:25:07'!
FloatArray removeSelector: #at2:put:!
!methodRemoval: FloatArray #at:put: stamp: 'len 11/19/2025 17:31:52'!
FloatArray removeSelector: #at:put:!
!methodRemoval: FloatArray #at: stamp: 'len 11/19/2025 17:31:32'!
FloatArray removeSelector: #at:!
