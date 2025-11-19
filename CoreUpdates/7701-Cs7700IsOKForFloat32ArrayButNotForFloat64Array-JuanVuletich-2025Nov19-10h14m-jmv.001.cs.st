'From Cuis7.5 [latest update: #7700] on 19 November 2025 at 10:17:17 am'!

!Float64Array methodsFor: 'accessing' stamp: 'jmv 5/18/2016 10:50'!
at: index
	"Return the element (e.g., 64 bit Float) at the given index.
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits."
	| answer |

	answer := Float new.
	answer replaceWordsFrom: 1 to: 2 with: self startingAt: index * 2 - 1.
	^answer! !

!Float64Array methodsFor: 'accessing' stamp: 'dtl 5/24/2021 14:17:31'!
at: index put: aNumber
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as BoxedFloat64. I.e. a BoxedFloat64 and a Float64Array of size 1 hold the same bits."

	"This breaks with SmallFloat64"
	"self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat asFloat startingAt: 1."

	"Float >>basicAt: acts as if Floats were stored in big endian format. Our instances are in platform endianess."
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

