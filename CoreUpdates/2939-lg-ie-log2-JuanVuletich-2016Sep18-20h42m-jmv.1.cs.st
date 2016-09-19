'From Cuis 4.2 of 25 July 2013 [latest update: #2938] on 18 September 2016 at 8:46:43 pm'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 9/18/2016 20:46:06'!
lg
	"Answer the base 2 log of the receiver.
	Alternative notation. See https://en.wikipedia.org/wiki/Binary_logarithm"

	^self log2! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 9/18/2016 20:43:53'!
log2
	"
	(1/(2 raisedTo: 215)) log2
	(1/((2 raisedTo: 215)+(2 raisedTo: 213))) log2
	"
	^self isPowerOfTwo
		ifTrue: [ 1-denominator highBitOfMagnitude ]
		ifFalse: [ super log2 ]! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 9/18/2016 20:42:20'!
log2
	"
	(2 raisedTo: 215) log2
	((2 raisedTo: 215)+(2 raisedTo: 213)) log2
	It would be cool to be able to give integer answers for other bases when appropriate...
	"
	^self isPowerOfTwo
		ifTrue: [ self highBitOfMagnitude-1 ]
		ifFalse: [ super log2 ]! !

