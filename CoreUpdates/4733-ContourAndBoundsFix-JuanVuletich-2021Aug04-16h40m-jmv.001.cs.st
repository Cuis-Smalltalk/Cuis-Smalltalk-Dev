'From Cuis 5.0 [latest update: #4732] on 4 August 2021 at 5:11:38 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 17:02:09'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	aRectangle is clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				"Use the bounds bound by #drawOn. This method must therefore be called after calling #drawOn:."
				privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
				self wantsContour ifTrue: [
					 (aCanvas
						contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
						into: [ :contourArray :contourY0 :contourY1 |
							self setProperty: #contour toValue: contourArray.
							self setProperty: #contourY0 toValue: contourY0.
							self setProperty: #contourY1 toValue: contourY1.
						]).
				] ifFalse: [
					self setProperty: #contour toValue: nil.
					self setProperty: #contourY0 toValue: nil.
					self setProperty: #contourY1 toValue: nil.
				]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 17:06:52'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	To be called after #postDrawOn:. See also #displayBoundsSetFrom:
	This more complicated update is needed because the contour information from #drawOn: was possibly
	when drawing submorphs. So, the controur from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				| oldContour oldTop oldBottom |
				oldContour _ self valueOfProperty: #contour.
				oldTop _ self valueOfProperty: #contourY0.
				oldBottom _ self valueOfProperty: #contourY1.
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
				self wantsContour ifTrue: [
					(aCanvas
						contourOfCurrentMorphAfterDrawUpdate: oldContour
						oldTop: oldTop
						oldBottom: oldBottom
						into: [ :contourArray :contourY0 :contourY1 |
							self setProperty: #contour toValue: contourArray.
							self setProperty: #contourY0 toValue: contourY0.
							self setProperty: #contourY1 toValue: contourY1.
						])]]]].! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/4/2021 17:04:08'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	(aMorph displayBoundsIntersects: (privateDisplayBounds outsetBy: 1)) ifFalse: [
		^false ].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/4/2021 17:11:18'!
contourIntersects: aContourArray top: aContourTop bottom: aContourBottom
	"Check if contours intersect.
	If contour is not available, use displayBounds"

	| contour contourTop contourBottom x0Own x1Own x0Arg x1Arg |
	contour _ self valueOfProperty: #contour.	
	contourTop _ (self valueOfProperty: #contourY0) ifNil: [aContourTop].
	contourBottom _ (self valueOfProperty: #contourY1) ifNil: [aContourBottom].

	(contourTop max: aContourTop) to: (contourBottom min: aContourBottom) do: [ :y |
		x0Own _ contour ifNil: [privateDisplayBounds left] ifNotNil: [ contour at: (y - contourTop) * 2 + 1 ].
		x1Own _ contour ifNil: [privateDisplayBounds right-1] ifNotNil: [ contour at: (y - contourTop) * 2 + 2 ].
		x0Arg _ aContourArray at: (y - aContourTop) * 2 + 1.
		x1Arg _ aContourArray at: (y - aContourTop) * 2 + 2.
		(x0Own <= x1Arg and: [ x0Arg <= x1Own ])
			ifTrue: [ ^true ]].

	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/4/2021 17:06:14'!
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
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
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

