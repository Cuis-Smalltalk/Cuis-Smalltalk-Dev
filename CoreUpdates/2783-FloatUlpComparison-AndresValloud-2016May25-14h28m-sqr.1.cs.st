'From Cuis 4.2 of 25 July 2013 [latest update: #2778] on 26 May 2016 at 6:23:21.636751 pm'!

!Number methodsFor: 'comparing' stamp: 'sqr 5/26/2016 17:56'!
isWithin: anInteger ulpsFrom: aFloat

	^self asFloat isWithin: anInteger ulpsFrom: aFloat! !


!Float methodsFor: 'comparing' stamp: 'sqr 5/25/2016 15:35'!
isWithin: anInteger ulpsFrom: aNumber

	| aFloat ulpDistance |
	aFloat := aNumber asFloat.
	ulpDistance := aFloat ulp * anInteger.
	^self
		between: ulpDistance negated + aFloat
		and: ulpDistance + aFloat! !


!Float methodsFor: 'comparing' stamp: 'sqr 5/25/2016 15:23'!
closeTo: num
 	"are these two numbers close?"
	num isNumber ifFalse: [^super closeTo: num].
	self = 0.0 ifTrue: [^num abs < 0.0001].
	num = 0 ifTrue: [^self abs < 0.0001].
	^self = num asFloat
		or: [(self - num) abs / (self abs max: num abs) < 0.0001]! !

!Float methodsFor: 'mathematical functions' stamp: 'sqr 5/26/2016 17:54'!
arcTan: denominator
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Implementation note: use sign in order to catch cases of negativeZero"

	^self = 0.0
		ifTrue: [denominator sign >= 0
			ifTrue: [ 0.0 ]
			ifFalse: [ self sign >= 0
				ifTrue: [ Pi ]
				ifFalse: [ Pi negated ]]]
		ifFalse: [denominator = 0.0
			ifTrue: [self > 0.0
				ifTrue: [ Halfpi ]
				ifFalse: [ Halfpi negated ]]
			ifFalse: [denominator > 0
				ifTrue: [ (self / denominator) arcTan ]
				ifFalse: [self > 0
					ifTrue: [ ((self / denominator) arcTan) + Pi ]
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]! !

