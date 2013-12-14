'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 12 December 2013 at 11:06:51.22671 am'!

!Integer methodsFor: 'mathematical functions' stamp: 'sqr 12/12/2013 10:50'!
productTo: anInteger
	"Answer self * (self + 1) * ... * anInteger"
	
	| integers answer |
	integers := anInteger - self + 1.
	integers < 1 ifTrue: [^1].
	integers > 7 ifTrue:
		[
			| half |
			half := integers bitShift: -1.
			^(self productTo: self + half)
				* (self + half + 1 productTo: anInteger)
		].
	answer := self.
	self + 1 to: anInteger do:
		[:each | answer := answer * each].
	^answer! !

!Integer methodsFor: 'mathematical functions' stamp: 'sqr 12/12/2013 10:59'!
sqrtRounded

	| sqrtFloor |
	sqrtFloor := (self bitShift: 2) sqrtFloor.
	^(sqrtFloor bitShift: -1) + (sqrtFloor bitAnd: 1)! !

!Integer methodsFor: 'mathematical functions' stamp: 'sqr 12/12/2013 11:03'!
sumTo: anInteger
	"Answer self + self + 1 + ... + anInteger"
	
	| integers |
	integers := anInteger - self + 1.
	integers < 1 ifTrue: [^0].
	^self + self + integers - 1 * integers bitShift: -1! !


!Integer methodsFor: 'mathematical functions' stamp: 'sqr 12/12/2013 10:37'!
factorial

	self < 0 ifTrue: [self error: 'Undefined for negative integers'].
	^1 productTo: self! !

!Integer methodsFor: 'mathematical functions' stamp: 'sqr 12/12/2013 10:40'!
take: anInteger
	"Answer the binomial coefficient (self anInteger)"
	" 6 take: 3  "

	(anInteger < 0 or: [anInteger > self]) ifTrue: [^0].
	^(self - anInteger + 1 productTo: self) // anInteger factorial! !

