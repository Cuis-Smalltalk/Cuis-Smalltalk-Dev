'From Cuis7.5 [latest update: #7538] on 21 September 2025 at 11:27:26 am'!

!PlacedMorph methodsFor: 'geometry' stamp: 'hlsf 9/21/2025 11:27:09'!
rotateBy: radians
	"Change the rotation of this morph. Argument is an angle (possibly negative).
	Note: the rotation is done around our #rotationCenter. This mimics the behavior of the rotation halo handle."

	| r |
	r := self yAxisPointsUp ifTrue: [ radians negated ] ifFalse: [ radians ].
	location := (location composedWith: (
		AffineTransformation withRadians: r around: self rotationCenter)).
	self fixYAxisDirection.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

