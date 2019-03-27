'From Cuis 5.0 of 7 November 2016 [latest update: #3688] on 27 March 2019 at 10:21:01 am'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 3/26/2019 12:03:11'!
arcTan: denominator
	"Answer the angle in radians.
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
			ifFalse: [denominator > 0.0
				ifTrue: [ (self / denominator) arcTan ]
				ifFalse: [self > 0.0
					ifTrue: [ ((self / denominator) arcTan) + Pi ]
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]! !

