'From Cuis 5.0 of 7 November 2016 [latest update: #3099] on 10 June 2017 at 1:39:18 am'!

!Array methodsFor: 'printing' stamp: 'pb 6/10/2017 01:39:04'!
isLiteral
	"Definition from Squeak"
	^ self class == Array and: [
		self allSatisfy: [ :each |
			each isLiteral ]].! !

