'From Cuis 5.0 [latest update: #3830] on 2 August 2019 at 9:27:45 am'!

!BoxedFloat64 methodsFor: 'testing' stamp: 'jmv 8/1/2019 16:42:44'!
isFinite
	"simple, byte-order independent test for rejecting Not-a-Number and (Negative)Infinity"

	^(self - self) = 0.0! !

!BoxedFloat64 methodsFor: 'testing' stamp: 'jmv 8/1/2019 16:42:00'!
isInfinite
	"Return true if the receiver is positive or negative infinity."

	^ self = Infinity or: [self = NegativeInfinity]! !

!BoxedFloat64 methodsFor: 'testing' stamp: 'jmv 8/1/2019 16:39:56'!
isNaN
	"simple, byte-order independent test for Not-a-Number"

	^ self ~= self! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 16:59:41'!
arTanh
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self = 1 ifTrue: [^ Float infinity].
	self = -1 ifTrue: [^Float negativeInfinity].
	self abs > 1 ifTrue: [^ Float nan].
	^((1 + self) / (1 - self)) ln / 2! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 17:02:16'!
sinh
	"Answer receivers hyperbolic sine"
	
	| ex |
	ex _ self abs exp.
	^ (ex - ex reciprocal) / 2 * self sign! !

!Float methodsFor: 'testing' stamp: 'jmv 8/1/2019 16:43:28'!
isFinite
	"Infinities and Not a Number are only represented as BoxedFloat64"

	^ true! !

!Float methodsFor: 'testing' stamp: 'jmv 8/2/2019 09:22:50'!
isInfinite
	"Infinities are only represented as BoxedFloat64"

	^ false
! !

!Float methodsFor: 'testing' stamp: 'jmv 8/1/2019 16:40:18'!
isNaN
	"Not a Number are only represented as BoxedFloat64"

	^ false! !


!BoxedFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 8/1/2019 18:39:12'!
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

	| positive |
	<primitive: 53>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue: 
			[positive _ (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent! !


!SmallFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 8/1/2019 18:39:17'!
exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See comment at BoxedFloat64"

	| positive |
	<primitive: 553>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue: 
			[positive _ (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent! !


!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 8/1/2019 16:57:21'!
readFrom: aStream
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"

	| sign days hours minutes seconds nanos nanosBuffer |
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	days := (aStream upTo: $:) asInteger * sign.
	hours := (aStream upTo: $:) asInteger * sign.
	minutes := (aStream upTo: $:) asInteger * sign.
	seconds := (aStream upTo: $.) asInteger * sign.
	nanosBuffer := '000000000' copy.
	nanos := WriteStream on: nanosBuffer.
	[aStream atEnd not and: [aStream peek isDigit]]
		whileTrue: [nanos nextPut: aStream next].
		
	^ self 
		days: days 
		hours: hours 
		minutes: minutes 
		seconds: seconds 
		nanoSeconds: (nanosBuffer asInteger * sign)

	"	'0:00:00:00' asDuration
		'0:00:00:00.000000001' asDuration
		'0:00:00:00.999999999' asDuration
		'0:00:00:00.100000000' asDuration
		'0:00:00:00.001 ' asDuration
		'0:00:00:00.1' asDuration
		'0:00:00:01 ' asDuration
		'0:12:45:45' asDuration
		'1:00:00:00' asDuration
		'365:00:00:00' asDuration
		'-7:09:12:06.10' asDuration
		'+0:01:02:3' asDuration
 	"! !

!methodRemoval: Float #sign:!
Float removeSelector: #sign:!
