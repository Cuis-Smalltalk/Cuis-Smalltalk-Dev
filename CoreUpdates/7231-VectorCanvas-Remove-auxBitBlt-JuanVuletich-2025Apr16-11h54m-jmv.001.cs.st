'From Cuis7.3 [latest update: #7140] on 16 April 2025 at 12:03:44 pm'!

!WorldMorph commentStamp: 'jmv 4/16/2025 11:44:38' prior: 0!
A WorldMorph is a kind of PasteUpMorph that can act as the root morph of a Morphic system. Usually uses the whole Display.

activeHand 				-- a HandMorph
hands 					-- Array of HandMorphs (typically only one) representing the Cursor and its event queue.
canvas 					-- BitBlitCanvas on the DisplayScreen. (Display getCanvas)
damageRecorder 		-- the DamageRecorder for the Display
stepList 					-- a Heap of StepMessage.  Each morph which wants stepping has a StepMessage here.
								See comment in #Morph>>stepAt:
lastStepTime 			-- 'now' as last sampled at step. (Time localMillisecondClock)
lastStepMessage 		-- nil or a selector
lastCycleTime 			-- 'now' as last sampled (Time localMillisecondClock)
alarms 					-- a Heap of MorphicAlarm.  See comment in #Morph>>addAlarm:after:
								An _alarm_ is an action to be done once, vs a _step_ which is cycled.
lastAlarm 				-- 'now' as sampled at last alarm (Time localMillisecondClock).
drawingFailureMorphs 	-- a WeakIdentitySet of Morphs with drawing failures
waitDelay 				-- a Delay
pause 					-- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate.
lastCycleHadAnyEvent 	-- a boolean
!

!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'clippingSpecStack csi boundsFinderCanvas auxBitBltEngine attachedCanvas '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 4/16/2025 11:42:47'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	^ aString asUnicodeString drawOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !


!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 4/16/2025 12:02:22'!
image: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resampled image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

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
			srcBox := srcForm boundingBox.
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (srcBox translatedBy: aPoint).
			dstBox := dstBox origin extent: dstBox extent + 1.1.
			dstBox := ((currentTransformation externalBoundingRectOf: dstBox))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 4/16/2025 12:03:10'!
image: srcForm at: aPoint sourceRect: sourceRect
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resampled image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

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
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (currentTransformation externalBoundingRectOf: (srcBox translatedBy: aPoint-0.5))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 4/16/2025 11:52:50'!
setForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing"
	"
	self runningWorld setupNewCanvasAndEngine
	"
	aForm depth = 32 ifFalse: [ self error: 'VectorCanvas can only work on 32 bit target' ].
	form := aForm.
	engine := VectorEngine onForm: form subPixelAntiAliasing: aBoolean.
	engine canvas: self.
	self newClipRect: nil.! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 4/16/2025 11:53:07'!
setVectorEngine: vectorEngineToReuse

	form := vectorEngineToReuse target.
	engine := vectorEngineToReuse.
	self doOwnEngine.
	self newClipRect: nil.! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 4/16/2025 11:52:44'!
nilState
	"Nil possibly trouble making references. See senders."
	super nilState.
	boundsFinderCanvas notNil ifTrue: [
		boundsFinderCanvas nilState ].! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 4/16/2025 11:52:40'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle.
	boundsFinderCanvas ifNotNil: [
		boundsFinderCanvas setClipRect: aRectangle ].! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 4/16/2025 11:52:24'!
privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
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
		fillRect: (rect insetBy: borderWidth).! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 4/16/2025 11:51:45'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor
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
		frameRect: rect borderWidth: borderWidth.! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 4/16/2025 11:51:12'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
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
		frameRect: rect borderWidth: borderWidth.! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 4/16/2025 11:43:10'!
prefFont

	^ #(
		#(defaultFontSize 18 SmallInteger defaultFontSize:)
		#(aaFontsColormapDepth 4)
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)! !

!methodRemoval: VectorCanvas #drawString:from:to:atBaseline:font:color: stamp: 'jmv 4/16/2025 11:54:30'!
VectorCanvas removeSelector: #drawString:from:to:atBaseline:font:color:!
!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'clippingSpecStack csi boundsFinderCanvas attachedCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
