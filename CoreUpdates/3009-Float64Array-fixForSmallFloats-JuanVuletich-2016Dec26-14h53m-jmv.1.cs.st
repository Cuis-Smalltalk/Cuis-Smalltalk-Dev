'From Cuis 5.0 of 7 November 2016 [latest update: #3008] on 26 December 2016 at 2:54:38 pm'!

!Float64Array methodsFor: 'accessing' stamp: 'jmv 12/26/2016 14:53:06'!
floatAt: index put: aFloat
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as BoxedFloat64. I.e. a BoxedFloat64 and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:put:"

	"This breaks with SmallFloat64"
	"self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat asFloat startingAt: 1."

	"Float >>basicAt: acts as if Floats were stored in big endian format. Our instances are in platform endianess."
	Smalltalk isLittleEndian
		ifTrue: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 2).
			self basicAt: index * 2 put: (aFloat basicAt: 1) ]
		ifFalse: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 1).
			self basicAt: index * 2 put: (aFloat basicAt: 2) ].
	^aFloat! !

