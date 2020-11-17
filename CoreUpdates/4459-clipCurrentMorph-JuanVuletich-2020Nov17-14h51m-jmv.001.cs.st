'From Cuis 5.0 [latest update: #4453] on 17 November 2020 at 2:53:21 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/17/2020 14:51:05'!
clipCurrentMorph: aBoolean! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 11/17/2020 14:51:56'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphs ifFalse: [ ^aBlock value ].

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	"Might use the fact that currentMorph has just been drawn."
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw.
	self setClipRect: (prevClipRect intersect: clippingMorphDisplayBounds).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

!methodRemoval: MorphicCanvas #clippingMorph: stamp: 'jmv 11/17/2020 14:52:29'!
MorphicCanvas removeSelector: #clippingMorph:!
