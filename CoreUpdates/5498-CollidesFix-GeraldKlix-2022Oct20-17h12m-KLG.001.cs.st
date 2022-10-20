'From Haver 6.0 [latest update: #5496] on 20 October 2022 at 5:32:52 pm'!

!Morph methodsFor: 'geometry services' stamp: 'KLG 9/21/2022 09:52:56'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the BoxedMorph hierarchy should answer #true to wantsContour."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	aMorph displayFullBounds
		ifNil: [ ^false ]
		ifNotNil: [ :r |
			(self displayFullBounds
				ifNil: [ ^ false ]
				ifNotNil: [ :myFullBounds | myFullBounds intersects: r ]) ifFalse: [ ^false ]].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

