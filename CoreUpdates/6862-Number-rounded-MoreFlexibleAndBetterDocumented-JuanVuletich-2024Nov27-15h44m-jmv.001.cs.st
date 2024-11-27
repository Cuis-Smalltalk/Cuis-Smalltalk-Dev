'From Cuis7.1 [latest update: #6861] on 27 November 2024 at 4:11:48 pm'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 11/27/2024 15:58:02'!
roundedHTE
	"Answer the integer nearest the receiver.
		(-3/2) roundedHTE. -2 .
		(-5/2) roundedHTE. -2 .
		(3/2) roundedHTE. 2 .
		(5/2) roundedHTE. 2 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	
	See more detailed comment at Float >> #roundedHTE"

	| truncated fractionPartAbs |
	truncated := self truncated.
	fractionPartAbs := (self-truncated) abs.
	fractionPartAbs = `1/2`
		ifTrue: [ truncated even ifTrue: [^truncated] ifFalse: [^truncated + self sign]].
	fractionPartAbs < `1/2`
		ifTrue: [^ truncated]
		ifFalse: [^ truncated + self sign]! !


!Float methodsFor: 'truncation and round off' stamp: 'jmv 11/27/2024 15:57:32'!
roundedHTE
	"Answer the integer nearest the receiver.
		-1.5 roundedHTE. -2 .
		-2.5 roundedHTE. -2 .
		1.5 roundedHTE. 2 .
		2.5 roundedHTE. 2 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Extract from Wikipedia as of Nov 27, 2024:
		Rounding half to even

		This is a tie-breaking rule without positive/negative bias and without bias toward/away from zero. By this convention, if the fractional part of x is 0.5, then y is the even integer nearest to x. Thus, for example, 23.5 becomes 24, as does 24.5; however, −23.5 becomes −24, as does −24.5. This function minimizes the expected error when summing over rounded figures, even when the inputs are mostly positive or mostly negative, provided they are neither mostly even nor mostly odd.

		This variant is also called convergent rounding, statistician's rounding, Dutch rounding, Gaussian rounding, odd–even rounding, or bankers' rounding.

		This is the default rounding mode used in IEEE 754 operations for results in binary floating-point formats.

		By eliminating bias, repeated addition or subtraction of independent numbers, as in a one-dimensional random walk, will give a rounded result with an error that tends to grow in proportion to the square root of the number of operations rather than linearly.

		However, this rule distorts the distribution by increasing the probability of evens relative to odds. Typically this is less important than the biases that are eliminated by this method. 
	"

	| truncated fractionPart |
	truncated := self truncated.
	fractionPart := self - truncated.
	(fractionPart > -0.5 and: [fractionPart < 0.5])
		ifTrue: [^truncated].
	fractionPart > 0.5
		ifTrue: [ ^truncated + 1 ].
	fractionPart < -0.5
		ifTrue: [ ^truncated - 1 ].
	truncated even ifTrue: [^truncated].
	self > 0
		ifTrue: [ ^truncated + 1 ]
		ifFalse: [ ^truncated - 1 ]! !


!Integer methodsFor: 'truncation and round off' stamp: 'jmv 11/27/2024 15:45:01'!
roundedHTE
	"Refer to the comment in Number|roundedHTE."

	^self! !


!Number methodsFor: 'truncation and round off' stamp: 'jmv 11/27/2024 16:09:38'!
rounded
	"Answer the integer nearest the receiver.
	There are two rounding strategies available in Cuis:
	
	#roundedHAFZ: What most people assume as the standard way of rounding
	#roundedHTE: A more careful strategy. Default rounding used in IEEE 754 binary Floating Point
	
	Note: Feel free to redefine this method to use a different rounding mode.
	"

	^self roundedHTE! !

!Number methodsFor: 'truncation and round off' stamp: 'jmv 11/27/2024 15:50:51'!
roundedHAFZ
	"Answer the integer nearest the receiver.
	Use the rounding rule commonly taught in school and most familiar to most people.
		(-3/2) roundedHAFZ. -2 .
		(-5/2) roundedHAFZ. -3 .
		(3/2) roundedHAFZ. 2 .
		(5/2) roundedHAFZ. 3 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero
	See #rounded"

	^(self + (self sign / 2)) truncated.! !


!Float methodsFor: 'truncation and round off' stamp: 'jmv 11/27/2024 16:05:39'!
roundedHAFZ
	"Answer the integer nearest the receiver.
	Use the rounding rule commonly taught in school and most familiar to most people.
		-1.5 roundedHAFZ. -2 .
		-2.5 roundedHAFZ. -3 .
		1.5 roundedHAFZ. 2 .
		2.5 roundedHAFZ.  3 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero

	Extract from Wikipedia as of Nov 27, 2024:

This method, also known as commercial rounding, treats positive and negative values symmetrically, and therefore is free of overall positive/negative bias if the original numbers are positive or negative with equal probability. It does, however, still have bias away from zero. 

	See #rounded"

	self fractionPart abs < 0.5
		ifTrue: [^self truncated]
		ifFalse: [^self truncated + self sign].! !

!methodRemoval: Integer #rounded stamp: 'jmv 11/27/2024 16:01:39'!
Integer removeSelector: #rounded!
!methodRemoval: Float #rounded stamp: 'jmv 11/27/2024 16:01:32'!
Float removeSelector: #rounded!
