'From Cuis 5.0 [latest update: #4529] on 9 February 2021 at 11:59:05 am'!

!Float methodsFor: 'truncation and round off' stamp: 'jmv 2/8/2021 19:08:52'!
exponent
	"Usually run as a primitive in specific subclass.
	Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two  or between -1 and -2 (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Note: invalid for infinities, NaN and zero.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand . f exponent. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"

	self = 0.0 ifTrue: [^MinValLogBase2-1].
	self isFinite ifFalse: [^Float emax+1].
	self isDenormalized ifTrue: [^MinValLogBase2 + self mantissaPart asFloat exponent].
	^self exponentPart! !


!BoxedFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 2/8/2021 18:54:45'!
exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two  or between -1 and -2 (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand . f exponent. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"

	<primitive: 53>
	^super exponent! !


!SmallFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 2/8/2021 18:54:52'!
exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See comment at BoxedFloat64"

	<primitive: 553>
	^super exponent! !

