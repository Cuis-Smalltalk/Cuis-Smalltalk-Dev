'From Cuis7.3 [latest update: #7070] on 11 March 2025 at 3:11:24 pm'!
!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine attachedCanvas csi clippingSpecStack '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 3/7/2025 16:05:02'!
clipCurrentMorph: aBoolean
	| prevClipSpec thisClipSpec |
	aBoolean
		ifTrue: 
		[
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
		]
		ifFalse: 
		[
			csi := csi-1.
			thisClipSpec := clippingSpecStack at: csi.
		].! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 3/7/2025 10:35:49'!
initializeWithTranslation: aPoint
	| h w initialClipSpec |

	super initializeWithTranslation: aPoint.
	engine geometryTransformation: currentTransformation.

	h := form height.
	w := form width.
	initialClipSpec := IntegerArray new: h * 2.
	0 to: form height-1 do: [ :y |
		initialClipSpec at: y * 2 + 1 put: 0.
		initialClipSpec at: y * 2 + 2 put: w ].
	clippingSpecStack := OrderedCollection with: initialClipSpec.
	csi := 1.

	boundsFinderCanvas := BoundsFinderCanvas new initializeWithTranslation: aPoint.
	"So bounds found are exactly the same as our engine would find."
	boundsFinderCanvas engine antiAliasingWidth: engine antiAliasingWidth.
	boundsFinderCanvas engine subPixelDelta: engine subPixelDelta.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !

!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'clippingSpecStack csi boundsFinderCanvas auxBitBltEngine attachedCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| e initialClipSpec |
VectorCanvas allSubInstancesDo: [ :canvas |
	canvas form ifNotNil: [
		e := canvas extent.
		initialClipSpec := IntegerArray new: e y * 2.
		0 to: e y-1 do: [ :y |
			initialClipSpec at: y * 2 + 1 put: 0.
			initialClipSpec at: y * 2 + 2 put: e x ].
		canvas instVarNamed: 'clippingSpecStack' put:​ (OrderedCollection with: initialClipSpec).
		canvas instVarNamed: 'csi' put:​ 1 ]].!

