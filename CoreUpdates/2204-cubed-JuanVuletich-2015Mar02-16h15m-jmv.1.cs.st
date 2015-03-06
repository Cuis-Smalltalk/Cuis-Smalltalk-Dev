'From Cuis 4.2 of 25 July 2013 [latest update: #2203] on 2 March 2015 at 5:16:54.82882 pm'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 3/2/2015 16:15'!
cubed
	"Answer the receiver multipled by itself."

	^self * self * self! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 3/2/2015 16:15'!
cubed
	"See Fraction (Number) | cubed"
	^ Fraction numerator: numerator cubed denominator: denominator cubed! !

