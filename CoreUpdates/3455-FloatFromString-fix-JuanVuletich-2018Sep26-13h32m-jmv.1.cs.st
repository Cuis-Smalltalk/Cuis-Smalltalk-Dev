'From Cuis 5.0 of 7 November 2016 [latest update: #3454] on 26 September 2018 at 1:32:50 pm'!

!Number class methodsFor: 'instance creation' stamp: 'jmv 9/26/2018 13:28:05'!
readRemainderOf: integerPart from: aStream base: base withSign: sign
	"Read optional fractional part and exponent, and return the final result"
	| value fraction fracpos peekChar exp scale convertToFloat |

	convertToFloat := false.
	value := integerPart.
	(aStream peekFor: $.)
		ifTrue: [		 "<integer>.<fraction>"
			(aStream atEnd not and: [ aStream peek digitValue between: 0 and: base - 1 ])
				ifTrue:  [
					fracpos := aStream position.
					fraction := Integer readFrom: aStream base: base.
					fraction := fraction / (base raisedToInteger: aStream position - fracpos).
					value := value + fraction.
					convertToFloat := true ]
				ifFalse: [
					"oops - just <integer>."
					aStream skip: -1.
					"un-gobble the period"
					^ value * sign"Number readFrom: '3r-22.2'"]].
	peekChar := aStream peek.
	peekChar = $e | (peekChar = $d) | (peekChar = $q)
		ifTrue: [			"<number>(e|d|q)<exponent>>"
			aStream next.
			(aStream atEnd not and: [ (aStream peek digitValue between: 0 and: 9) or: [ aStream peek = $- ]])
				ifTrue:  [
					exp := Integer readFrom: aStream.
					scale := base raisedToInteger: exp.
					value := value * scale ]
				ifFalse: [
					"oops - just <number>."
					aStream skip: -1.]].
	^convertToFloat
		ifTrue: [
			(value = 0.0 and: [ sign = -1 ])
				ifTrue: [ Float negativeZero ]
				ifFalse: [ (value * sign) asFloat ]]
		ifFalse: [ value * sign ]! !


!Float methodsFor: 'printing' stamp: 'nice 4/20/2010 22:48'!
absPrintExactlyOn: aStream base: base
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
	This version guarantees that the printed representation exactly represents my value
	by using exact integer arithmetic."

	| significand exp baseExpEstimate r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount slowbit shead |
	self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
	significand := self significandAsInteger.
	roundingIncludesLimits := significand even.
	"What follows is equivalent, but faster than
		exp := self exponentPart - 52."
	exp := (self exponent - 52) max: MinValLogBase2.
	baseExpEstimate := (self exponent * base asFloat reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[significand ~= 16r10000000000000
				ifTrue:
					[r := significand bitShift: 1 + exp.
					s := 2.
					mPlus := mMinus := 1 bitShift: exp]
				ifFalse:
					[r := significand bitShift: 2 + exp.
					s := 4.
					mPlus := 2 * (mMinus := 1 bitShift: exp)]]
		ifFalse:
			[(exp = MinValLogBase2 or: [significand ~= 16r10000000000000])
				ifTrue:
					[r := significand bitShift: 1.
					s := 1 bitShift: 1 - exp.
					mPlus := mMinus := 1]
				ifFalse:
					[r := significand bitShift: 2.
					s := 1 bitShift: 2 - exp.
					mPlus := 2.
					mMinus := 1]].
	baseExpEstimate >= 0
		ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale := base raisedToInteger: baseExpEstimate negated.
			r := r * scale.
			mPlus := mPlus * scale.
			mMinus := mMinus * scale].
	((r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[r := r * base.
			mPlus := mPlus * base.
			mMinus := mMinus * base].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	slowbit := 1 - s lowBit .
	shead := s bitShift: slowbit.
	[d := (r bitShift: slowbit) // shead.
	r := r - (d * s).
	(tc1 := (r > mMinus) not and: [roundingIncludesLimits or: [r < mMinus]]) |
	(tc2 := (r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * base.
		mPlus := mPlus * base.
		mMinus := mMinus * base.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[(tc1 not or: [r * 2 >= s]) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]! !

