'From Cuis 5.0 [latest update: #4659] on 8 July 2021 at 12:16:12 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/8/2021 12:09:56'!
contourIntersects: aContourArray top: aContourTop bottom: aContourBottom
	"Check if contours intersect.
	If contour is not available, use displayBounds"

	| contour contourTop contourBottom x0Own x1Own x0Arg x1Arg |
	contour _ self valueOfProperty: #contour.	
	contourTop _ privateDisplayBounds top max: 0.
	contourBottom _ privateDisplayBounds bottom-1.

	(contourTop max: aContourTop) to: (contourBottom min: aContourBottom) do: [ :y |
		x0Own _ contour ifNil:[privateDisplayBounds left] ifNotNil: [ contour at: (y - contourTop) * 2 + 1 ].
		x1Own _ contour ifNil:[privateDisplayBounds right-1] ifNotNil: [ contour at: (y - contourTop) * 2 + 2 ].
		x0Arg _ aContourArray at: (y - aContourTop) * 2 + 1.
		x1Arg _ aContourArray at: (y - aContourTop) * 2 + 2.
		(x0Own <= x1Arg and: [ x0Arg <= x1Own ])
			ifTrue: [ ^true ]].

	^false! !

