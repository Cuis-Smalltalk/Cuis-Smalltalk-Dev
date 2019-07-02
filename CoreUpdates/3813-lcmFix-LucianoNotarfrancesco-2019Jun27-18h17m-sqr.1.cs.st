'From Cuis 5.0 of 7 November 2016 [latest update: #3795] on 28 June 2019 at 10:45:24 am'!

!Integer methodsFor: 'mathematical functions' stamp: 'len 6/17/2019 04:28:05'!
lcm: anInteger
	"Answer the least common multiple of the receiver and anInteger.
	This is the smallest non-negative integer divisible by the receiver and the argument.
	If either the receiver or the argument is zero, the result is zero."

	(self = 0 or: [anInteger = 0]) ifTrue: [^ 0].
	^self abs // (self gcd: anInteger) * anInteger abs! !

