'From Cuis 6.0 [latest update: #5105] on 18 April 2022 at 10:52:39 am'!

!LayoutSpec methodsFor: 'testing' stamp: 'jmv 4/18/2022 10:51:25'!
isProportionalHeight
	"Zero actuaqlly means fixed height."

	^ proportionalHeight notNil and: [ proportionalHeight isZero not ].! !

!LayoutSpec methodsFor: 'testing' stamp: 'jmv 4/18/2022 10:50:58'!
isProportionalWidth
	"Zero actuaqlly means fixed width."

	^ proportionalWidth notNil and: [ proportionalWidth isZero not ].! !

