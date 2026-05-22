"
A HybridCanvas works as a BitBltCanvas until faced with a situation that BitBlitCanvas can't handle. At that point, the vectorCanvas takes command.

self runningWorld setCanvas: (HybridCanvas onForm: Display).
self runningWorld canvas class
"
Class {
	#name : 'HybridCanvas',
	#superclass : 'BitBltCanvas',
	#instVars : [
		'vectorCanvas',
		'morphIDsForm',
		'drawingOnMorphIDs'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'accessing' }
HybridCanvas >> boundsFinderCanvas [
	^vectorCanvas boundsFinderCanvas
]

{ #category : 'testing' }
HybridCanvas >> canDoVectorGraphics [
	^true
]

{ #category : 'private' }
HybridCanvas >> canvasToUse [

	currentMorph ifNotNil: [
		currentMorph requiresVectorCanvas ifTrue: [
			^ vectorCanvas ]].
	^self.
]

{ #category : 'private' }
HybridCanvas >> currentMorph: aMorph drawingMorphStack: morphs currentTransformation: tx transformations: txs cti: i [
	"For syncing to our VectorCanvas"
	currentMorph := aMorph.
	drawingMorphStack := morphs.
	currentTransformation := tx.
	transformations := txs.
	cti := i.
]

{ #category : 'private' }
HybridCanvas >> doOwnEngine [
	"VectorCanvas needs this."
	vectorCanvas doOwnEngine
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawCloseIcon [
	vectorCanvas  drawCloseIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawCollapseIcon [
	vectorCanvas  drawCollapseIcon
]

{ #category : 'morphic' }
HybridCanvas >> drawCurrentAndSubmorphs [
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self onMorphIdsDo: [
			currentMorph drawOn: self ].
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph ].

	"Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(current postDrawOn: self) ifTrue: [
			self onMorphIdsDo: [
				current postDrawOn: self ].
			self displayBoundsUpdateOnCurrentMorph ].
		current isHighlighted ifTrue: [
			self drawHighlight: current ]].
]

{ #category : 'morphic' }
HybridCanvas >> drawCurrentAsError [

	vectorCanvas drawCurrentAsError
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawDownIcon [
	vectorCanvas  drawDownIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawExpandIcon [
	vectorCanvas  drawExpandIcon
]

{ #category : 'morphic' }
HybridCanvas >> drawHand [

	vectorCanvas drawHand
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawLeftIcon [
	vectorCanvas  drawLeftIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawMenuIcon [
	vectorCanvas  drawMenuIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawPushPinIcon [
	vectorCanvas  drawPushPinIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawRightIcon [
	vectorCanvas  drawRightIcon
]

{ #category : 'drawing-windows' }
HybridCanvas >> drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight [

	self
		windowFrame: aRectangle
		color: borderColor
		radius: Theme current roundedWindowRadius
		border: borderWidth
		topColor: titleAreaColor
		labelHeight: labelHeight + borderWidth
		insideColor: insideColor
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawSquareCloseIcon [
	vectorCanvas  drawSquareCloseIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawSquareExpandIcon [
	vectorCanvas  drawSquareExpandIcon
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawSquareMenuIcon [
	vectorCanvas  drawSquareMenuIcon
]

{ #category : 'drawing-text' }
HybridCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor [

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
]

{ #category : 'drawing-text' }
HybridCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc [

	^vectorCanvas drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc
]

{ #category : 'drawing-text' }
HybridCanvas >> drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor [

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
]

{ #category : 'drawing-text' }
HybridCanvas >> drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine [

	drawingOnMorphIDs ifTrue: [
		^nil ].
	^super drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: avoidLastNotFullyVisibleLine
]

{ #category : 'drawing-icons' }
HybridCanvas >> drawUpIcon [
	vectorCanvas  drawUpIcon
]

{ #category : 'morphic world' }
HybridCanvas >> drawWorldBackground: aWorldMorph rects: worldBackgroundDamage [
	"Draw worldBackgroundDamage rects for aWorldMorph.
	Do not include submorphs."
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	worldBackgroundDamage do: [ :r |
		self newClipRect: r.
		aWorldMorph drawOn: self.
		self onMorphIdsDo: [
			aWorldMorph drawOn: self ]].
]

{ #category : 'drawing-rectangles' }
HybridCanvas >> fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor [
	| rect |

	drawingOnMorphIDs ifTrue: [
		aRectangle hasPositiveExtent ifFalse: [^ self].
		rect := (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.
		engine combinationRule: 0.
		engine fillRect: rect.
		^self ].

	super fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
]

{ #category : 'drawing-images' }
HybridCanvas >> image: aForm at: aPoint sourceRect: sourceRect [
	"Draw a translucent image using the best available way of representing translucency."
	| p e |

	drawingOnMorphIDs ifTrue: [
		"We transform p but not e. This is on purpose. We want to affect exactly the same rect as super."
		e := sourceRect extent.
		e > `0@0` ifFalse: [ ^self ].
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		engine combinationRule: 0.
		engine fillRect: (p extent: e).
		^self ].

	super image: aForm at: aPoint sourceRect: sourceRect
]

{ #category : 'initialization' }
HybridCanvas >> initializeWithTranslation: aPoint [

	boundsFinderCanvas := BitBltBoundsFinderCanvas new initializeWithTranslation: aPoint.
	vectorCanvas initializeWithTranslation: aPoint.
]

{ #category : 'morphic' }
HybridCanvas >> into: aMorph [
	vectorCanvas into: aMorph
]

{ #category : 'other' }
HybridCanvas >> isConsistentWithTarget [
	^vectorCanvas isConsistentWithTarget
]

{ #category : 'morphic' }
HybridCanvas >> morphIdAt: aPoint [

	^ vectorCanvas morphIdAt: aPoint
]

{ #category : 'private' }
HybridCanvas >> nilState [
	"Nil possibly trouble making references. See senders."
	super nilState.
	morphIDsForm := nil.
]

{ #category : 'private' }
HybridCanvas >> onMorphIdsDo: aBlock [

	engine instVarNamed: 'destForm' put: morphIDsForm.
	drawingOnMorphIDs := true.
	aBlock ensure: [
		engine instVarNamed: 'destForm' put: form.
		drawingOnMorphIDs := false ].
]

{ #category : 'morphic' }
HybridCanvas >> outOfMorph [
	vectorCanvas outOfMorph
]

{ #category : 'other' }
HybridCanvas >> pinArrays [
	vectorCanvas pinArrays.
	super pinArrays.
]

{ #category : 'morphic' }
HybridCanvas >> restorePatch: savedPatch bounds: savedBounds [
	"Argument must previously be obtained by calling #savePatch:bounds:"

	vectorCanvas restorePatch: savedPatch bounds: savedBounds
]

{ #category : 'morphic' }
HybridCanvas >> savePatch: prevSavedPatch bounds: aRectangle [
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:bounds:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	^ vectorCanvas savePatch: prevSavedPatch bounds: aRectangle
]

{ #category : 'private' }
HybridCanvas >> setClipRect: aRectangle [
	super setClipRect: aRectangle.
	vectorCanvas setClipRect: aRectangle
]

{ #category : 'private' }
HybridCanvas >> setForm: aForm subPixelAntiAliasing: aBooleanOrNil [
	"nil means use default kind of anti aliasing"

	form := aForm.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas := VectorCanvas onForm: form.
	vectorCanvas attachedCanvas: self.
	morphIDsForm := Form extent: form extent depth: 32 bits: vectorCanvas engine morphIds.
	drawingOnMorphIDs := false.
	self newClipRect: nil.
]

{ #category : 'private' }
HybridCanvas >> setPaintColor: aColor [

	drawingOnMorphIDs ifTrue: [
		engine sourceForm: nil.
		engine combinationRule: 0.
		^self ].

	super setPaintColor: aColor
]

{ #category : 'private' }
HybridCanvas >> setVectorEngine: vectorEngineToReuse [

	form := vectorEngineToReuse target.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas := VectorCanvas basicNew
		setVectorEngine: vectorEngineToReuse.
	vectorCanvas attachedCanvas: self.
	morphIDsForm := Form extent: form extent depth: 32 bits: vectorCanvas engine morphIds.
	drawingOnMorphIDs := false.
	self newClipRect: nil.
]

{ #category : 'other' }
HybridCanvas >> unpinAndFreeMemory [
	vectorCanvas unpinAndFreeMemory.
	super unpinAndFreeMemory.
]

{ #category : 'testing' }
HybridCanvas >> usesVectorEnginePlugin [
	^vectorCanvas usesVectorEnginePlugin
]

{ #category : 'accessing' }
HybridCanvas >> vectorEngine [
	"Answer our VectorEngineDrawer, or nil if we don't have one."
	^vectorCanvas vectorEngine
]

{ #category : 'initialization' }
HybridCanvas >> world: aWorldMorph [
	super world: aWorldMorph.
	vectorCanvas world: aWorldMorph
]
