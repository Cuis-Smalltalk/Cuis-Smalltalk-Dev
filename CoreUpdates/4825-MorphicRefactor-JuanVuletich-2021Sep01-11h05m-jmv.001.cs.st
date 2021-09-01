'From Cuis 5.0 [latest update: #4822] on 1 September 2021 at 11:34:10 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 9/1/2021 11:26:37'!
contourOfCurrentMorphAfterDrawInto: anArray into: aBlock
	"We don't compute contours. See other implementors."! !


!Morph methodsFor: 'geometry' stamp: 'jmv 9/1/2021 11:33:22'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas canvasToUse]) ifTrue: [
				privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw.
				self wantsContour
					ifTrue: [
						 (aCanvas
							contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
							into: [ :contourArray :contourY0 :contourY1 |
								self setProperty: #contour toValue: contourArray.
								self setProperty: #contourY0 toValue: contourY0.
								self setProperty: #contourY1 toValue: contourY1 ])]
					ifFalse: [
						self setProperty: #contour toValue: nil.
						self setProperty: #contourY0 toValue: nil.
						self setProperty: #contourY1 toValue: nil ]]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/1/2021 11:33:11'!
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
			(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas]) ifTrue: [
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw.
				self wantsContour ifTrue: [
					(aCanvas
						contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
						oldTop: (self valueOfProperty: #contourY0)
						oldBottom: (self valueOfProperty: #contourY1)
						into: [ :contourArray :contourY0 :contourY1 |
							self setProperty: #contour toValue: contourArray.
							self setProperty: #contourY0 toValue: contourY0.
							self setProperty: #contourY1 toValue: contourY1 ])]]]].! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 9/1/2021 11:14:29'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restrinction. See inherintance, with VectorGraphics package loaded."

	| boundingRect |
	boundingRect _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 9/1/2021 11:27:45'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphsReally ifFalse: [ ^aBlock value ].

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

!methodRemoval: BitBltCanvas #boundingRectOfCurrentMorphAfterDraw stamp: 'jmv 9/1/2021 11:14:47'!
BitBltCanvas removeSelector: #boundingRectOfCurrentMorphAfterDraw!
!methodRemoval: BitBltBoundsFinderCanvas #boundingRectOfCurrentMorphAfterDraw stamp: 'jmv 9/1/2021 11:14:43'!
BitBltBoundsFinderCanvas removeSelector: #boundingRectOfCurrentMorphAfterDraw!
!methodRemoval: MorphicCanvas #boundingRectOfCurrentMorphFromLocalBounds stamp: 'jmv 9/1/2021 11:30:30'!
MorphicCanvas removeSelector: #boundingRectOfCurrentMorphFromLocalBounds!
