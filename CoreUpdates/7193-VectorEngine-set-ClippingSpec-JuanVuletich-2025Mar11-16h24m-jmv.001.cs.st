'From Cuis7.3 [latest update: #7076] on 11 March 2025 at 4:45:23 pm'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:41:53'!
clipByCurrentMorphStart! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:42:18'!
clipByCurrentMorphStop! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:41:56'!
clipByCurrentMorphStart! !

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:42:22'!
clipByCurrentMorphStop! !


!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:43:24'!
clipByCurrentMorphStart
	| prevClipSpec thisClipSpec |
	prevClipSpec := clippingSpecStack at: csi.
	csi := csi+1.
	clippingSpecStack size < csi
		ifTrue: [
			thisClipSpec := prevClipSpec copy.
			clippingSpecStack add: thisClipSpec ]
		ifFalse: [
			thisClipSpec := clippingSpecStack at: csi.
			thisClipSpec fillWith: prevClipSpec ].
	engine morphContourAfterDrawInterectInto: thisClipSpec.
	engine clippingSpec: thisClipSpec.! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:43:51'!
clipByCurrentMorphStop
	| prevClipSpec |
	csi := csi-1.
	prevClipSpec := clippingSpecStack at: csi.
	engine clippingSpec: prevClipSpec.! !


!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/11/2025 16:34:24'!
clippingSpec: aClippingSpec
	clippingSpec := aClippingSpec.! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 3/11/2025 16:44:13'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphs ifFalse: [ ^aBlock value ].

	prevClipRect := clipRect.
	prevClippingMorphRect := clippingMorphDisplayBounds.
	clippingMorphDisplayBounds := self boundingRectOfCurrentMorphAfterDraw insetBy: 1.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipByCurrentMorphStart.
	aBlock ensure: [
		self clipByCurrentMorphStop.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds := prevClippingMorphRect ]! !

!methodRemoval: VectorCanvas #clipCurrentMorph: stamp: 'jmv 3/11/2025 16:44:19'!
VectorCanvas removeSelector: #clipCurrentMorph:!
!methodRemoval: BoundsFinderCanvas #clipCurrentMorph: stamp: 'jmv 3/11/2025 16:44:19'!
BoundsFinderCanvas removeSelector: #clipCurrentMorph:!
!methodRemoval: MorphicCanvas #clipCurrentMorph: stamp: 'jmv 3/11/2025 16:44:20'!
MorphicCanvas removeSelector: #clipCurrentMorph:!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
