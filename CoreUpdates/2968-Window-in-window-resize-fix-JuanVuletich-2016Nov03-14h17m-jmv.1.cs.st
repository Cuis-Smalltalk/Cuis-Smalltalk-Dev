'From Cuis 4.2 of 25 July 2013 [latest update: #2967] on 3 November 2016 at 2:17:56 pm'!

!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 11/3/2016 14:14:27'!
adjustOwnerAt: aPoint
	| p |
	owner ifNotNil: [
		p _ aPoint + 1.
		owner owner ifNotNil: [ :parent |
			p _ parent internalizeFromWorld: p ].
		owner perform: selector with: (p perform: coordinateGetter) ]! !

