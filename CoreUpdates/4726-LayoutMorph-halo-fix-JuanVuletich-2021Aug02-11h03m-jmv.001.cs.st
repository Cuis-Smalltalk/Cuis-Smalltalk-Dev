'From Cuis 5.0 [latest update: #4725] on 2 August 2021 at 11:05:07 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/2/2021 11:04:42'!
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
							self wantsContour ifTrue: [ aCanvas contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) ]).
						privateDisplayBounds hasPositiveExtent ifFalse: [		  "This might happen for morphs with empty #drawOn: like, for example, LahoutMorphs."
							privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphFromLocalBounds ]
						]]]].! !

