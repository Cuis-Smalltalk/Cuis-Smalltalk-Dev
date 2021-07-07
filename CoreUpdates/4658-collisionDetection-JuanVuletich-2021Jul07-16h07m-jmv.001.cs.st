'From Cuis 5.0 [latest update: #4656] on 7 July 2021 at 4:07:59 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/7/2021 15:27:25'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	(aMorph displayBoundsIntersects: (privateDisplayBounds outsetBy: 1)) ifFalse: [
		^false ].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ privateDisplayBounds top max: 0.
		contourBottom _ privateDisplayBounds bottom-1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/7/2021 15:50:00'!
contourIntersects: aContourArray top: aContourTop bottom: aContourBottom
	"Precise check with contour, if available.
	If contour is not available, use displayBounds"

	| contour contourTop contourBottom x0Own x1Own x0Arg x1Arg |
	contour _ self valueOfProperty: #contour.	
	contourTop _ privateDisplayBounds top max: 0.
	contourBottom _ privateDisplayBounds bottom-1.

	"Check if contours intersect, or touch horizontally"
	(contourTop max: aContourTop) to: (contourBottom min: aContourBottom) do: [ :y |
		x0Own _ contour ifNil:[privateDisplayBounds left] ifNotNil: [ contour at: (y - contourTop) * 2 + 1 ].
		x1Own _ contour ifNil:[privateDisplayBounds right-1] ifNotNil: [ contour at: (y - contourTop) * 2 + 2 ].
		x0Arg _ aContourArray at: (y - aContourTop) * 2 + 1.
		x1Arg _ aContourArray at: (y - aContourTop) * 2 + 2.
		(x0Own <= x1Arg and: [ x0Arg <= x1Own ])
			ifTrue: [ ^true ]].

	"Check if aContour bottom touches our top and is above us"
	contourTop-1 = aContourBottom ifTrue: [
		x0Own _ contour ifNil:[privateDisplayBounds left] ifNotNil: [ contour at: 1 ].
		x1Own _ contour ifNil:[privateDisplayBounds right-1] ifNotNil: [ contour at: 2 ].
		x0Arg _ aContourArray at: (aContourBottom - aContourTop) * 2 + 1.
		x1Arg _ aContourArray at: (aContourBottom - aContourTop) * 2 + 2.
		(x0Own < x1Arg and: [ x0Arg < x1Own ])
			ifTrue: [ ^true ]].

	"Check if aContour top touches our bottom and is below us"
	aContourTop-1 = contourBottom ifTrue: [
		x0Own _ contour ifNil:[privateDisplayBounds left] ifNotNil: [ contour at: (contourBottom - contourTop) * 2 + 1 ].
		x1Own _ contour ifNil:[privateDisplayBounds right-1] ifNotNil: [ contour at: (contourBottom - contourTop) * 2 + 2 ].
		x0Arg _ aContourArray at: 1.
		x1Arg _ aContourArray at: 2.
		(x0Own < x1Arg and: [ x0Arg < x1Own ])
			ifTrue: [ ^true ]].

	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/7/2021 15:22:13'!
displayBoundsIntersects: aRectangle

	privateDisplayBounds ifNil: [
		^false ].
	^privateDisplayBounds intersects: aRectangle! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/7/2021 14:56:58'!
isCloserThan: aNumber toPoint: aPoint
	"Answer true if our closest point to aPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	Uses precise testing of the morph contour if available. See #knowsContour."

	| center contourTop contourBottom |
	privateDisplayBounds ifNil: [
		^false ].
	center _ privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - aPoint y) abs < (privateDisplayBounds height  // 2 + aNumber) ifFalse: [
		^false ].
	(center x - aPoint x) abs < (privateDisplayBounds width // 2 + aNumber) ifFalse: [
		^false ].
	contourTop _ privateDisplayBounds top max: 0.
	contourBottom _ privateDisplayBounds bottom-1.
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		"Contour rows to consider are those within requested distance."
		y0 _ aPoint y - aNumber max: contourTop.
		y1 _ aPoint y + aNumber min: contourBottom.
		y0 to: y1 do: [ :y |
			x0 _ (contour at: (y - contourTop) * 2 + 1) - aNumber.
			x1 _ (contour at: (y - contourTop) * 2 + 2) + aNumber.
			"If a vertical line of 2*aNumber height centered on aPoint is inside the contour, quick exit"
			(aPoint x between: x0 and: x1) ifTrue: [ ^true ].
			"Check if aPoint is close enough to contour"
			(x0@y - aPoint) r < aNumber ifTrue: [ ^true ].
			(x1@y - aPoint) r < aNumber ifTrue: [ ^true ]].
		"Not inside, not close enough to contour"
		^ false ].
	"If contour is not available, and aPoint is close enough to displayBounds, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'testing' stamp: 'jmv 7/7/2021 11:56:01'!
isOwnedByWorldOrHand
	"I.e. are we a top morph?"

	^ self isOwnedByWorld or: [ self isOwnedByHand ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/7/2021 11:53:35'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				self isOrthoRectangularMorph
					ifTrue: [
						"In these cases, #drawOn: might not be able to find bounds (InnerTextMorph, for example),
						but it is OK to compute them from #morphLocalBounds. This is not true for non-orthoRectangular morphs!!"
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds ]
					ifFalse: [
						"In this cases, we use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
						privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
						self setProperty: #contour toValue: (
							self knowsContour ifTrue: [ aCanvas contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) ])]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/7/2021 14:57:23'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	To be called after #postDrawOn:. See also #displayBoundsSetFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				self isOrthoRectangularMorph ifFalse: [				"Like in #displayBoundsSetFrom:"
					| oldTop oldBottom |
					oldTop _ privateDisplayBounds top max: 0.
					oldBottom _ privateDisplayBounds bottom-1.
					privateDisplayBounds _ privateDisplayBounds
						quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
					self setProperty: #contour toValue:
						(self knowsContour ifTrue: [
							aCanvas
								contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
								oldTop: oldTop
								oldBottom: oldBottom ])]]]].! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/7/2021 11:56:38'!
knowsContour
	"True if a morph requires the Canvas to update a #morphContour property on us.
	Any subclass might answer true, if it #requiresVectorCanvas.
	By default, any top morph that is drawn by VectorCanvas."

	^self requiresVectorCanvas and: [ self isOwnedByWorldOrHand ]! !

