'From Cuis 4.2 of 25 July 2013 [latest update: #2214] on 30 March 2015 at 9:22:29.961 am'!

!Rectangle methodsFor: 'transforming' stamp: 'jmv 3/30/2015 09:20'!
flippedBy: direction centerAt: aPoint 
	"Return a copy flipped #vertical or #horizontal, about aPoint."
	| futureOrigin futureCorner |
	direction == #horizontal
		ifTrue:
			[futureOrigin := self topRight.
			futureCorner := self bottomLeft]
		ifFalse: [direction == #vertical
			ifTrue:
				[futureOrigin := self bottomLeft.
				futureCorner := self topRight]
			ifFalse: [self error: 'unrecognizable direction']].
	^ (futureOrigin flipBy: direction centerAt: aPoint)
		corner: (futureCorner flipBy: direction centerAt: aPoint)! !

