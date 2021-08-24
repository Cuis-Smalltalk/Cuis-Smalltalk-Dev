'From Cuis 5.0 [latest update: #4806] on 24 August 2021 at 7:37:30 pm'!

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 8/24/2021 19:36:56'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphsReally ifFalse: [ ^aBlock value ].

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	"Might use the fact that currentMorph has just been drawn."
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

