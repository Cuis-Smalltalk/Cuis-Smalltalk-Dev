'From Cuis 4.2 of 25 July 2013 [latest update: #1967] on 25 February 2014 at 9:59:18.808708 am'!

!Point methodsFor: 'comparing' stamp: 'jmv 2/18/2014 12:33'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]! !

!Point methodsFor: 'geometry' stamp: 'jmv 2/18/2014 14:40'!
barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: aBlock ifDegenerate: degeneratBlock
	"Return  the barycentric coordinates for the receiver within the triangle defined by the three arguments."
	| p0 b0 b1 b2 b3 |
	p0 _ self.
	b0 _ ((p2 x - p1 x) * (p3 y - p1 y)) - ((p3 x - p1 x) * (p2 y - p1 y)).
	b0 isZero ifTrue: [ ^ degeneratBlock value ]. "degenerate"
	b0 _ 1.0 / b0.
	b1 _ (((p2 x - p0 x) * (p3 y - p0 y)) - ((p3 x - p0 x) * (p2 y - p0 y))) * b0.
	b2 _ (((p3 x - p0 x) * (p1 y - p0 y)) - ((p1 x - p0 x) * (p3 y - p0 y))) * b0.
	b3 _ (((p1 x - p0 x) * (p2 y - p0 y)) - ((p2 x - p0 x) * (p1 y - p0 y))) * b0.
	^aBlock value: b1 value: b2 value: b3! !


!Point methodsFor: 'point functions' stamp: 'jmv 2/18/2014 14:42'!
insideTriangle: p1 with: p2 with: p3
	"Return true if the receiver is within the triangle defined by the three coordinates.
	Note: This method computes the barycentric coordinates for the receiver and tests those coordinates."
	self
		barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: [ :b1 :b2 :b3 | 
			b1 < 0.0 ifTrue: [ ^false ].
			b2 < 0.0 ifTrue: [ ^false ].
			b3 < 0.0 ifTrue: [ ^false ].
			^true ]
		ifDegenerate: [ ^false ]! !

