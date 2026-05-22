Class {
	#name : 'VectorCanvas',
	#superclass : 'AbstractVectorCanvas',
	#instVars : [
		'clippingSpecStack',
		'csi',
		'boundsFinderCanvas',
		'attachedCanvas'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'class initialization' }
VectorCanvas class >> initialize [
	UISupervisor whenUIinSafeState: [
		(VectorEngineWithPlugin isPluginAvailable
			ifTrue: [
				MorphicCanvas activeSubclass: HybridCanvas
				"MorphicCanvas activeSubclass: VectorCanvas"
				]
			ifFalse: [
				'VectorEnginePlugin (VM plugin) is not available. HybridCanvas will be activated.' print.
				MorphicCanvas activeSubclass: HybridCanvas ]) ].
]

{ #category : 'accessing' }
VectorCanvas >> attachedCanvas: aCanvas [
	^attachedCanvas := aCanvas
]

{ #category : 'accessing' }
VectorCanvas >> boundsFinderCanvas [
	^boundsFinderCanvas
]

{ #category : 'testing' }
VectorCanvas >> canDoVectorGraphics [
	^true
]

{ #category : 'morphic' }
VectorCanvas >> clipByCurrentMorphStart [
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
	engine clippingSpec: thisClipSpec.
]

{ #category : 'morphic' }
VectorCanvas >> clipByCurrentMorphStop [
	| prevClipSpec |
	csi := csi-1.
	prevClipSpec := csi = 1
		"Using nil instead of the initial clipping spec when no clipping is an optimization."
		ifFalse: [ clippingSpecStack at: csi ].
	engine clippingSpec: prevClipSpec.
]

{ #category : 'morphic' }
VectorCanvas >> contourOfCurrentMorphAfterDrawInto: anArray into: aBlock [
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or use a new array if not."

	engine morphContourAfterDrawInto: anArray into: aBlock.
]

{ #category : 'morphic' }
VectorCanvas >> contourOfCurrentMorphAfterDrawUpdate: anArray oldTop: oldTop oldBottom: oldBottom into: aBlock [
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or a new array if not."

	anArray
		ifNil: [ engine morphContourAfterDrawInto: nil into: aBlock ]
		ifNotNil: [
			engine morphContourAfterDrawUpdate: anArray 
				oldTop: oldTop oldBottom: oldBottom into: aBlock ].
]

{ #category : 'morphic' }
VectorCanvas >> contourSetOnCurrentMorph [
	"Set contour.
	To be called immediately after drawing currentMorph."

	currentMorph mainCanvas ifNotNil: [ :c |
		c canvasToUse == self ifTrue: [
			currentMorph wantsContour
				ifTrue: [
					self
						contourOfCurrentMorphAfterDrawInto: (currentMorph valueOfProperty: #contour) 
						into: [ :contourArray :contourY0 :contourY1 |
							currentMorph setProperty: #contour toValue: contourArray.
							currentMorph setProperty: #contourY0 toValue: contourY0.
							currentMorph setProperty: #contourY1 toValue: contourY1 ]]
				ifFalse: [
					self contourClearOnCurrentMorph ]]].
]

{ #category : 'morphic' }
VectorCanvas >> contourUpdateOnAllOwners [
	"Update contour on all owners of currentMorph.
	To also include the effect of #postDrawOn: and submorphs."

	currentMorph mainCanvas ifNotNil: [ :c |
		c canvasToUse = self ifTrue: [
			1 to: cti-1 do: [ :i |
				(drawingMorphStack at: i) ifNotNil: [ :m |
					m wantsContour ifTrue: [
						self
							contourOfCurrentMorphAfterDrawUpdate: (m valueOfProperty: #contour)
							oldTop: (m valueOfProperty: #contourY0)
							oldBottom: (m valueOfProperty: #contourY1)
							into: [ :contourArray :contourY0 :contourY1 |
								m setProperty: #contour toValue: contourArray.
								m setProperty: #contourY0 toValue: contourY0.
								m setProperty: #contourY1 toValue: contourY1 ]]]]]].
]

{ #category : 'morphic' }
VectorCanvas >> contourUpdateOnCurrentMorph [
	"Update contour.
	To also include the effect of #postDrawOn: and submorphs."

	currentMorph mainCanvas ifNotNil: [ :c |
		c canvasToUse == self ifTrue: [
			currentMorph wantsContour
				ifTrue: [
					self
						contourOfCurrentMorphAfterDrawUpdate: (currentMorph valueOfProperty: #contour)
						oldTop: (currentMorph valueOfProperty: #contourY0)
						oldBottom: (currentMorph valueOfProperty: #contourY1)
						into: [ :contourArray :contourY0 :contourY1 |
							currentMorph setProperty: #contour toValue: contourArray.
							currentMorph setProperty: #contourY0 toValue: contourY0.
							currentMorph setProperty: #contourY1 toValue: contourY1 ]]]].
]

{ #category : 'private' }
VectorCanvas >> doOwnEngine [
	"VectorCanvas needs this."
	engine canvas: self.
]

{ #category : 'morphic' }
VectorCanvas >> drawCurrentAndSubmorphs [

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
		self displayBoundsSetOnCurrentMorph.
		self contourSetOnCurrentMorph.
		self contourUpdateOnAllOwners ].

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
			self displayBoundsUpdateOnCurrentMorph.
			self contourUpdateOnCurrentMorph.
			self contourUpdateOnAllOwners ].
		current isHighlighted ifTrue: [
			self drawHighlight: current ]].
]

{ #category : 'morphic' }
VectorCanvas >> drawCurrentAsError [
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r := currentMorph localBoundsForError.
	w := r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	r := r insetBy: w.
	self line: r topLeft to: r bottomRight width: w color: `Color yellow`.
	self line: r topRight to: r bottomLeft width: w color: `Color yellow`.	
	self displayBoundsSetOnCurrentMorph.
	self contourSetOnCurrentMorph.
]

{ #category : 'drawing - MorphicCanvas protocol' }
VectorCanvas >> image: srcForm at: aPoint [
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resampled image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway."

	"We need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			| p bitBlt |
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			bitBlt := BitBltCanvasEngine toForm: form.
			bitBlt
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: 0; sourceY: 0;
				destX: p x destY: p y width: srcForm width height: srcForm height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			"srcBox := srcBox origin extent: srcBox extent - 1."
			srcBox := srcForm boundingBox.
			dstBox := currentTransformation externalBoundingRectOf: srcBox.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) floor ].
			"dstBox := srcBox translatedBy: aPoint."
			dstBox := aPoint extent: srcBox extent.
			dstBox := ((currentTransformation externalBoundingRectOf: dstBox))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].
]

{ #category : 'drawing - MorphicCanvas protocol' }
VectorCanvas >> image: srcForm at: aPoint sourceRect: sourceRect [
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resampled image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway."

	"We need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (sourceRect translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			| p bitBlt |
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			bitBlt := BitBltCanvasEngine toForm: form.
			bitBlt
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: sourceRect left; sourceY: sourceRect top;
				destX: p x destY: p y width: sourceRect width height: sourceRect height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			srcBox := sourceRect.
			"srcBox := srcBox origin extent: srcBox extent - 1."
			dstBox := currentTransformation externalBoundingRectOf: srcBox.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) floor ].
			"dstBox := srcBox translatedBy: aPoint."
			dstBox := aPoint extent: srcBox extent.
			dstBox := ((currentTransformation externalBoundingRectOf: dstBox))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].
]

{ #category : 'drawing - MorphicCanvas protocol' }
VectorCanvas >> image: aForm multipliedBy: aColor at: aPoint [
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	(BitBltCanvas onForm: Display) image: ((Form dotOfSize: 50) asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		BitBltCanvas accessProtect critical: [ | f |
			f := BitBltCanvas buildAuxWith: aForm multipliedWith: aColor.
			self image: f at: aPoint sourceRect: aForm boundingBox ]]
]

{ #category : 'drawing - MorphicCanvas protocol' }
VectorCanvas >> imageUnrotated: srcForm at: aPoint [
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	Scale and position like #image:at:, but don't rotate srcForm:
	Display it in its natural orientation."

	| cellSize srcBox dstBox srcQuad bb |

	"If no rotation or scaling, BitBlt is enough. But there's no need to repeat the code:
	If no rotation, 'unrotated' is the same as 'defaultlyRotated'."
	currentTransformation isPureTranslation ifTrue: [
		^self image: srcForm at: aPoint ].

	"Compute bounds, set morphIds for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.

	cellSize := 4.
	"srcBox := 0@0 corner: srcForm extent-1."
	srcBox := srcForm boundingBox.
	srcQuad := srcBox corners.
	dstBox := Rectangle
		center: (currentTransformation transform: srcBox center + aPoint) roundedHAFZ
		extent: (srcBox extent * currentTransformation scale) roundedHAFZ roundedToEven.
	bb := WarpBlt toForm: form.
	bb
		sourceForm: srcForm; combinationRule: Form blend;
		colorMap: (srcForm colormapIfNeededFor: form);
		clipRect: clipRect;
		cellSize: cellSize;
		copyQuad: srcQuad toRect: dstBox.

]

{ #category : 'initialization' }
VectorCanvas >> initializeWithTranslation: aPoint [
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
			cti: cti ]
]

{ #category : 'morphic' }
VectorCanvas >> into: aMorph [
	super into: aMorph.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]
]

{ #category : 'other' }
VectorCanvas >> isConsistentWithTarget [
	^engine isConsistentWithTarget
]

{ #category : 'morphic' }
VectorCanvas >> morphIdAt: aPoint [
	^engine morphIdAt: aPoint.
]

{ #category : 'private' }
VectorCanvas >> nilState [
	"Nil possibly trouble making references. See senders."
	super nilState.
	boundsFinderCanvas notNil ifTrue: [
		boundsFinderCanvas nilState ].
]

{ #category : 'morphic' }
VectorCanvas >> outOfMorph [
	super outOfMorph.
	attachedCanvas ifNotNil: [
		attachedCanvas
			currentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]
]

{ #category : 'other' }
VectorCanvas >> pinArrays [
	engine pinArrays.
	super pinArrays.
]

{ #category : 'private-framework' }
VectorCanvas >> privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor [
	"
	(VectorCanvas onForm: Display)
		privateFrameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color yellow
		borderWidth: 20
		color: Color green.
	Display forceToScreen
	"
	| bitBlt |
	bitBlt := BitBltCanvasEngine toForm: form.
	bitBlt clipRect: rect.
	bitBlt
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
]

{ #category : 'private-framework' }
VectorCanvas >> privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor [
	"
	(VectorCanvas onForm: Display)
		privateFrameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| bitBlt |
	bitBlt := BitBltCanvasEngine toForm: form.
	bitBlt clipRect: rect.
	bitBlt
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.
]

{ #category : 'private-framework' }
VectorCanvas >> privateFrameReverseGlobalRect: rect borderWidth: borderWidth [
	"
	(VectorCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| bitBlt |
	bitBlt := BitBltCanvasEngine toForm: form.
	bitBlt clipRect: rect.
	bitBlt
		sourceForm: nil;
		fillColor: `Color gray: 0.5`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth.
]

{ #category : 'private' }
VectorCanvas >> pvtDrawButtonFaceForm: aForm [
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	| color |
	color := currentMorph isPressed
		ifTrue: [ `Color gray: 0.75` ]
		ifFalse: [
			currentMorph mouseIsOver
				ifTrue: [ `Color gray: 0.75` ]
				ifFalse: [ `Color white` ]].

	self
		image: aForm
		multipliedBy: color
		at: (currentMorph morphExtent - aForm extent //2).
]

{ #category : 'morphic' }
VectorCanvas >> restorePatch: savedPatch bounds: savedBounds [
	"Argument must previously be obtained by calling #savePatch:bounds:"

	| bb bits |
	bb := BitBlt toForm: form.
	bb
		combinationRule: Form over;
		sourceX: 0; sourceY: 0;
		destX: savedBounds origin x destY: savedBounds origin y
			width: savedBounds width height: savedBounds height.
	bb sourceForm: savedPatch first;
		copyBits.
	bits := form bits.
	[
		form bits: engine morphIds.
		bb sourceForm: savedPatch second;
			copyBits.
	] ensure: [ form bits: bits ].
]

{ #category : 'morphic' }
VectorCanvas >> savePatch: prevSavedPatch bounds: aRectangle [
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:bounds:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| patchForm prevExtent extent savedIdsPatch bb answer bits |
	answer := prevSavedPatch.
	(answer is: #Array) ifFalse: [ answer := Array new: 2 ].

	prevExtent := 0@0.
	((answer first is: #Form) and: [
		prevExtent := answer first extent.
		prevExtent >= aRectangle extent]) 
			ifTrue: [
				patchForm := answer first.
				patchForm offset: 0@0.
				savedIdsPatch := answer second.
				savedIdsPatch offset: 0@0 ]
			ifFalse: [
				"allocate new patch form if needed"
				extent := aRectangle extent max: prevExtent.
				extent := (extent x // 64 + 2 * 64) @ (extent y + 64).						"Make it slightly larger, and even width."
				patchForm := Form extent: extent depth: form depth.
				savedIdsPatch := Form extent: extent depth: 32.
				answer at: 1 put: patchForm.
				answer at: 2 put: savedIdsPatch ].

	bb := BitBlt new.
	bb sourceForm: form;
		combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height.
	bb setDestForm: patchForm;
		copyBits.
	bits := form bits.
	[
		form bits: engine morphIds.
		bb setDestForm: savedIdsPatch;
			copyBits.
	] ensure: [ form bits: bits ].

	^answer
]

{ #category : 'private' }
VectorCanvas >> setClipRect: aRectangle [
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle.
	boundsFinderCanvas ifNotNil: [
		boundsFinderCanvas setClipRect: aRectangle ].
]

{ #category : 'initialization' }
VectorCanvas >> setForm: aForm subPixelAntiAliasing: aBoolean [
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setupNewCanvasAndEngine
	"
	aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ].
	form := aForm.
	engine := VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	self newClipRect: nil.
]

{ #category : 'initialization' }
VectorCanvas >> setVectorEngine: vectorEngineToReuse [

	form := vectorEngineToReuse target.
	engine := vectorEngineToReuse.
	self doOwnEngine.
	self newClipRect: nil.
]

{ #category : 'other' }
VectorCanvas >> unpinAndFreeMemory [
	engine unpinAndFreeMemory.
	super unpinAndFreeMemory.
]

{ #category : 'testing' }
VectorCanvas >> usesVectorEnginePlugin [
	^engine usesVectorEnginePlugin
]

{ #category : 'accessing' }
VectorCanvas >> vectorEngine [
	"Answer our VectorEngineDrawer, or nil if we don't have one."
	^engine
]

{ #category : 'initialization' }
VectorCanvas >> world: aWorldMorph [
	super world: aWorldMorph.
	boundsFinderCanvas world: aWorldMorph
]
