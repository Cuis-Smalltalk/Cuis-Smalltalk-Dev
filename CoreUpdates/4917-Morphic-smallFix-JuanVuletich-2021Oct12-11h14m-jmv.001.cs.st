'From Cuis 5.0 [latest update: #4911] on 12 October 2021 at 11:15:05 am'!

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 10/12/2021 11:06:35'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphsReally ifFalse: [ ^aBlock value ].

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw insetBy: 1.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

