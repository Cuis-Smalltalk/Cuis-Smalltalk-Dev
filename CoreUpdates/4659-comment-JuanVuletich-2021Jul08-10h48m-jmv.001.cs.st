'From Cuis 5.0 [latest update: #4658] on 8 July 2021 at 10:50:45 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/8/2021 10:48:34'!
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

