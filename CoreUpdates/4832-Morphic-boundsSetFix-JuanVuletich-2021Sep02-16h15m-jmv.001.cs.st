'From Cuis 5.0 [latest update: #4831] on 2 September 2021 at 4:31:06 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 9/2/2021 16:25:56'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas canvasToUse]) ifTrue: [
				privateDisplayBounds _ aCanvas canvasToUse boundingRectOfCurrentMorphAfterDraw.
				self wantsContour
					ifTrue: [
						 (aCanvas canvasToUse
							contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
							into: [ :contourArray :contourY0 :contourY1 |
								self setProperty: #contour toValue: contourArray.
								self setProperty: #contourY0 toValue: contourY0.
								self setProperty: #contourY1 toValue: contourY1 ])]
					ifFalse: [
						self setProperty: #contour toValue: nil.
						self setProperty: #contourY0 toValue: nil.
						self setProperty: #contourY1 toValue: nil ]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/2/2021 16:27:13'!
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
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas canvasToUse]) ifTrue: [
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas canvasToUse boundingRectOfCurrentMorphAfterDraw.
				self wantsContour ifTrue: [
					(aCanvas canvasToUse
						contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
						oldTop: (self valueOfProperty: #contourY0)
						oldBottom: (self valueOfProperty: #contourY1)
						into: [ :contourArray :contourY0 :contourY1 |
							self setProperty: #contour toValue: contourArray.
							self setProperty: #contourY0 toValue: contourY0.
							self setProperty: #contourY1 toValue: contourY1 ])]]]].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/2/2021 16:15:12'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restriction. See inherintance, with VectorGraphics package loaded."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

