'From Cuis 5.0 of 7 November 2016 [latest update: #3369] on 11 July 2018 at 6:10:38 pm'!

!Float methodsFor: 'printing' stamp: 'jmv 7/11/2018 18:06:31'!
absPrintOn: aStream base: base mantissaSignificantBits: significantBits
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion."

	| fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
	self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
	fBase := base asFloat.
	exp := self exponent.
	baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[r := self.
			s := 1.0.
			mPlus := 1.0 timesTwoPower: exp - significantBits.
			mMinus := self significand ~= 1.0 ifTrue: [mPlus] ifFalse: [mPlus / 2.0]]
		ifFalse:
			[r := self timesTwoPower: significantBits.
			s := 1.0 timesTwoPower:  significantBits.
			mMinus := 1.0 timesTwoPower: (exp max: -1024).
			mPlus :=
				(exp = MinValLogBase2) | (self significand ~= 1.0)
					ifTrue: [mMinus]
					ifFalse: [mMinus * 2.0]].
	baseExpEstimate >= 0
		ifTrue:
			[exp = 1023
				ifTrue:   "scale down to prevent overflow to Infinity during conversion"
					[r := r / fBase.
					s := s * (fBase raisedToInteger: baseExpEstimate - 1).
					mPlus := mPlus / fBase.
					mMinus := mMinus / fBase]
				ifFalse:
					[s := s * (fBase raisedToInteger: baseExpEstimate)]]
		ifFalse:
			[exp < -1023
				ifTrue:   "scale up to prevent denorm reciprocals overflowing to Infinity"
					[d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
					scale := fBase raisedToInteger: d.
					r := r * scale.
					mPlus := mPlus * scale.
					mMinus := mMinus * scale.
					scale := fBase raisedToInteger: (baseExpEstimate + d) negated]
				ifFalse:
				[scale := fBase raisedToInteger: baseExpEstimate negated].
			s := s / scale].
	(r + mPlus >= s)
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[s := s / fBase].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	[d := (r / s) truncated.
	r := r - (d * s).
	(tc1 := r <= mMinus) | (tc2 := r + mPlus >= s)] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * fBase.
		mPlus := mPlus * fBase.
		mMinus := mMinus * fBase.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2.0 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]! !

!Float methodsFor: 'printing' stamp: 'jmv 7/11/2018 17:45:59'!
printAsIEEE32BitPrecisionFloatOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:mantissaSignificantBits:
	Print as a 32 bit Float" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base mantissaSignificantBits: 24]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base mantissaSignificantBits: 24]]! !


!FloatArray methodsFor: 'printing' stamp: 'jmv 7/11/2018 17:49:59'!
printElementsOn: aStream
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When printing, print them as 32 bit Float."
	aStream nextPut: $(.
	self do: [ :element |
		element printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream space].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)! !


!Float methodsFor: 'printing' stamp: 'jmv 7/11/2018 17:43:34'!
absPrintOn: aStream base: base
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion."

	self absPrintOn: aStream base: base mantissaSignificantBits: 50 "approximately 3 lsb's of accuracy loss during conversion"! !

