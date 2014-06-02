'From Cuis 4.2 of 25 July 2013 [latest update: #1992] on 24 May 2014 at 11:47:41.189456 am'!

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 5/24/2014 11:47'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."

	| d n |
	n _ numerator nthRoot: aPositiveInteger.
	d _ denominator nthRoot: aPositiveInteger.
	"The #nthRoot: method in integer will only answer a Float if there's no exact nth root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat nthRoot: aPositiveInteger ].
	^n / d! !

