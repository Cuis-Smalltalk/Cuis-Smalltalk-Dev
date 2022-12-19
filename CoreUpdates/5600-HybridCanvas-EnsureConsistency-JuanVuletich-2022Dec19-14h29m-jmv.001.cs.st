'From Cuis 6.0 [latest update: #5599] on 19 December 2022 at 2:34:16 pm'!
!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine attachedCanvas '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 12/19/2022 11:36:46'!
attachedCanvas: aCanvas
	^attachedCanvas _ aCanvas! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 11:43:17'!
into: aMorph
	super into: aMorph.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 11:43:20'!
outOfMorph
	super outOfMorph.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 12/19/2022 11:40:59'!
currentMorph: aMorph drawingMorphStack: morphs currentTransformation: tx transformations: txs cti: i
	"For syncing to our VectorCanvas"
	currentMorph _ aMorph.
	drawingMorphStack _ morphs.
	currentTransformation _ tx.
	transformations _ txs.
	cti _ i.! !


!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 12/19/2022 11:44:01'!
initializeWithTranslation: aPoint

	super initializeWithTranslation: aPoint.
	engine geometryTransformation: currentTransformation.

	boundsFinderCanvas _ BoundsFinderCanvas new initializeWithTranslation: aPoint.
	"So bounds found are exactly the same as our engine would find."
	boundsFinderCanvas engine antiAliasingWidth: engine antiAliasingWidth.
	boundsFinderCanvas engine subPixelDelta: engine subPixelDelta.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 12/19/2022 11:42:18'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	super resetCanvas.
	boundsFinderCanvas resetCanvas.
	engine clearArrays.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !


!HybridCanvas methodsFor: 'initialization' stamp: 'jmv 12/19/2022 11:45:59'!
initializeWithTranslation: aPoint

	boundsFinderCanvas _ BitBltBoundsFinderCanvas new initializeWithTranslation: aPoint.
	vectorCanvas initializeWithTranslation: aPoint.! !

!HybridCanvas methodsFor: 'initialization' stamp: 'jmv 12/19/2022 11:48:09'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	boundsFinderCanvas resetCanvas.
	vectorCanvas resetCanvas.! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 12/19/2022 11:36:57'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

	form _ aForm.
	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas _ VectorCanvas onForm: form.
	vectorCanvas attachedCanvas: self.
	morphIDsForm _ Form extent: form extent depth: 32 bits: (vectorCanvas engine instVarNamed: 'morphIds').
	drawingOnMorphIDs _ false.
	self newClipRect: nil.! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 11:47:03'!
into: aMorph
	vectorCanvas into: aMorph! !

!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 12/19/2022 11:47:28'!
outOfMorph
	vectorCanvas outOfMorph! !

!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine attachedCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor ui ifNotNil: [ :world |world setMainCanvas ]!

