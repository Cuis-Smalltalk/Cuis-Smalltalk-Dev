'From Cuis 6.0 [latest update: #5089] on 25 February 2022 at 12:39:21 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 2/25/2022 12:38:57'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the BoxedMorph hierarchy should answer #true to wantsContour."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	aMorph displayFullBounds
		ifNil: [ ^false ]
		ifNotNil: [ :r |
			(self displayFullBounds intersects: r) ifFalse: [ ^false ]].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!methodRemoval: Morph #displayBoundsIntersects: stamp: 'jmv 2/25/2022 12:39:02'!
Morph removeSelector: #displayBoundsIntersects:!
