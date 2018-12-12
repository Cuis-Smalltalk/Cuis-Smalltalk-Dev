'From Cuis 5.0 of 7 November 2016 [latest update: #3515] on 4 December 2018 at 3:51:34 pm'!

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 15:21:29'!
nextAwayFromZero
	"Answer the Float with smallest magnitude but larger than ours, with the same sign
	Only for finite numbers."

	| exponent mantissa |
	self partValues: [ :sign :myExponent :myMantissa |
		myMantissa = 16r1FFFFFFFFFFFFF
			ifTrue: [
				mantissa _ 16r10000000000000.
				exponent _ myExponent +1 ]
			ifFalse: [
				mantissa _ myMantissa+1.
				exponent _ myExponent ].
		^ Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 15:21:25'!
nextTowardsZero
	"Answer the Float with largest magnitude but smaller than ours, with the same sign.
	Only for finite, non zero numbers."

	| exponent mantissa |
	self partValues: [ :sign :myExponent :myMantissa |
		myMantissa isZero ifTrue: [ self error: 'Not for zero values' ].
		(myMantissa = 16r10000000000000 and: [myExponent > Float emin])
			ifTrue: [
				mantissa _ 16r1FFFFFFFFFFFFF.
				exponent _ myExponent -1 ]
			ifFalse: [
				mantissa _ myMantissa-1.
				exponent _ myExponent ].
		^ Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !


!Float class methodsFor: 'instance creation' stamp: 'jmv 12/4/2018 14:10:38'!
signBit: signBit mantissaBits: mantissaBits exponentBits: exponentBits 
	"
	Float signBit: Float pi signBit mantissaBits: Float pi mantissaBits exponentBits: Float pi exponentBits
	"
	| answer leastSignificativeWord mostSignificativeWord |
	answer _ BoxedFloat64 new.
	mostSignificativeWord _ (signBit bitShift: 31) + (exponentBits bitShift: 20) + (mantissaBits bitShift: -32).
	leastSignificativeWord _ mantissaBits bitAnd: 16rFFFFFFFF.
	answer basicAt: 1 put: mostSignificativeWord.
	answer basicAt: 2 put: leastSignificativeWord.
	^ answer! !

!Float class methodsFor: 'instance creation' stamp: 'jmv 12/4/2018 14:09:38'!
signPart: signPart mantissaPart: mantissaPart exponentPart: exponentPart
	"
	Float signPart: Float pi signPart mantissaPart: Float pi mantissaPart exponentPart: Float pi exponentPart
	"
	^ signPart * mantissaPart asFloat timesTwoPower: exponentPart-52! !


!Float methodsFor: 'converting' stamp: 'jmv 12/4/2018 13:37:51'!
partBits: aThreeArgumentBlock
	"
	Extract the bits for Sign, Mantissa and Exponent parts of the Floating Point representation.
	Just extract the bits. Do not add implicit bit. Do not correct denormals. Do not substract exponent bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	| signBit exponentBits mantissaBits leastSignificativeWord mostSignificativeWord |

	mostSignificativeWord _ self basicAt: 1.
	leastSignificativeWord _ self basicAt: 2.
	signBit _ mostSignificativeWord bitShift: -31 .
	exponentBits _ (mostSignificativeWord bitShift: -20 ) bitAnd: 16r7FF.
	mantissaBits _ ((mostSignificativeWord bitAnd: 16r000FFFFF) bitShift: 32) + leastSignificativeWord.

	"Evaluate the block"
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 14:54:42'!
predecessor
	"Answer the largest Float smaller than self"

	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self isZero ifTrue: [
		"Both for positive and negative zero"
		^ -0.0 nextAwayFromZero ].
	^self > 0.0
		ifTrue: [ self nextTowardsZero ]
		ifFalse: [ self nextAwayFromZero ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 14:54:36'!
successor
	"Answer the smallest Float greater than self"

	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self isZero ifTrue: [
		"Both for positive and negative zero"
		^ 0.0 nextAwayFromZero ].
	^self < 0.0
		ifTrue: [ self nextTowardsZero ]
		ifFalse: [ self nextAwayFromZero ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 14:51:27'!
ulp
	"Answer the unit of least precision of the receiver"
	
	self isFinite ifFalse: [^self abs].
	^ (self nextAwayFromZero - self) abs! !

