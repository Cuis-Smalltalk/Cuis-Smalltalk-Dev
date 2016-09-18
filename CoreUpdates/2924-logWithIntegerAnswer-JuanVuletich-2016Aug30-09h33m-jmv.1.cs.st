'From Cuis 4.2 of 25 July 2013 [latest update: #2923] on 30 August 2016 at 10:24:53 am'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:04:34'!
log2
	"Answer the base-2 logarithm of the receiver.
	See comment at #log:"

	^self asFloat log2! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 09:51:26'!
log2
	"Answer the base 2 logarithm of the receiver."

	^ self ln / Ln2! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:07:23'!
log: aNumber
	"Answer the log base aNumber of the receiver."

	^ self ln / aNumber ln! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 09:43:44'!
log2
	"
	(1/(2 raisedTo: 215)) log2
	(1/((2 raisedTo: 215)+(2 raisedTo: 213))) log2
	"
	^self isPowerOfTwo
		ifTrue: [ denominator log2 negated ]
		ifFalse: [ super log2 ]! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:18:54'!
log: aNumber
	"Answer the log base aNumber of the receiver.
	(1/(3 raisedTo: 215)) log: 3
	(1/((3 raisedTo: 215)+(3 raisedTo: 213))) log: 3
	"
	^numerator = 1
		ifTrue: [ (denominator log: aNumber) negated ]
		ifFalse: [ super log: aNumber ]! !

!Fraction methodsFor: 'testing' stamp: 'jmv 8/29/2016 09:35:12'!
isPowerOfTwo
	^numerator = 1 and: [ denominator isPowerOfTwo ]! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 09:47:35'!
floatLog
	"Float, even if Integer result possible"
	| res h |
	res := super log.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 log * h + (self / (1 << h)) asFloat log! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/29/2016 10:12:55'!
log2
	"
	(2 raisedTo: 215) log2
	((2 raisedTo: 215)+(2 raisedTo: 213)) log2
	Estaria bueno poder calcular asi para otras bases... no solo 2!!
	Aunque sea caro, que sea entero!!
	"
	^self isPowerOfTwo
		ifTrue: [ self highBitOfMagnitude-1 ]
		ifFalse: [ super log2 ]! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:22:29'!
log: aNumber
	"Answer the log base aNumber of the receiver.
	(3 raisedTo: 215) log: 3
	((3 raisedTo: 215)+(3 raisedTo: 213)) log: 3
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| floatAnswer roundedAnswer |
	self <= 0 ifTrue: [DomainError signal: 'log is only defined for x > 0'].
	floatAnswer _ self asFloat log: aNumber.
	roundedAnswer _ floatAnswer rounded.
	(aNumber raisedToInteger: roundedAnswer) = self
		ifTrue: [ ^roundedAnswer ].
	^floatAnswer! !


!Collection methodsFor: 'math functions' stamp: 'jmv 8/30/2016 09:37:11'!
log2
	^ self collect: [:each | each log2]! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:04:37'!
ln
	"Answer the natural log of the receiver.
	See comment at #log:"

	^self asFloat ln! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:04:41'!
log
	"Answer the base-10 log of the receiver.
	See comment at #log:"

	^self asFloat log! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:09:47'!
log: aNumber
	"Answer the log base aNumber of the receiver.
	Note 1:
		Answer an Integer number if receiver and base are both integer (receiver might also be a fraction of the form 1/Integer), and the correct answer is integer too.
	Note2:
		There are several implementors of #log:, #log, #log2 and #ln in the Number hierarchy. Behavior is not consistent when receiver is a negative value. Some implementors answer Float nan, others raise a DomainError. Another possibility would be to answer the correct Complex number.
		It would be good to pick one and make it consistently followed.
	"

	^self asFloat log: aNumber! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:20:10'!
log
	"
	(1/(10 raisedTo: 215)) log
	(1/((10 raisedTo: 215)+(10 raisedTo: 213))) log
	"
	| res |
	self <= 0 ifTrue: [DomainError signal: 'log is only defined for x > 0'].

	"Integer answer if possible!!"
	numerator = 1
		ifTrue: [ ^denominator log negated ].

	"This because super log might overflow."
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [ ^self reciprocal log negated ].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/30/2016 10:22:52'!
log
	"This function is defined because super log might overflow.
	(10 raisedTo: 215) log
	((10 raisedTo: 215)+(10 raisedTo: 213)) log
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| floatAnswer roundedAnswer |
	self <= 0 ifTrue: [DomainError signal: 'log is only defined for x > 0'].
	floatAnswer _ self floatLog.
	roundedAnswer _ floatAnswer rounded.
	(10 raisedToInteger: roundedAnswer) = self
		ifTrue: [ ^roundedAnswer ].
	^floatAnswer! !

