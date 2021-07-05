'From Cuis 5.0 [latest update: #4651] on 5 July 2021 at 2:45:39 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/5/2021 10:20:27'!
knowsContour
	"True if a morph requires the Canvas to update a #morphContour property on us.
	Any subclass might answer true, if it #requiresVectorCanvas."

	^false! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/5/2021 10:43:23'!
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
						self knowsContour ifTrue: [
							self setProperty: #contour
								toValue: (aCanvas contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour)).
						]
						]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/5/2021 14:01:16'!
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
					oldTop _ privateDisplayBounds top.
					oldBottom _ privateDisplayBounds bottom-1.
					privateDisplayBounds _ privateDisplayBounds
						quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
					self knowsContour ifTrue: [
						self setProperty: #contour
							toValue: (aCanvas
								contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
								oldTop: oldTop
								oldBottom: oldBottom) ]]]]].! !

