'From Cuis 4.2 of 25 July 2013 [latest update: #2605] on 30 November 2015 at 12:48:57.245151 pm'!

!Integer methodsFor: 'mathematical functions' stamp: 'sqr 11/28/2015 15:15'!
sumTo: anInteger
	"Answer self + self + 1 + ... + anInteger"
	
	| integers |
	integers := anInteger - self + 1.
	integers < 1 ifTrue: [^0].
	^self + anInteger * integers bitShift: -1! !

