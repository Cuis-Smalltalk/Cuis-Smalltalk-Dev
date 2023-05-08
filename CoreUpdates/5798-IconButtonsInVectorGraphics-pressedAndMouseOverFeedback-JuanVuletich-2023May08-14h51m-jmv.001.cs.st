'From Cuis 6.0 [latest update: #5797] on 8 May 2023 at 2:55:38 pm'!

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 5/8/2023 14:42:20'!
image: srcForm at: aPoint sourceRect: sourceRect
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need tow of them!!"
	currentMorph == world ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: sourceRect left; sourceY: sourceRect top;
			destX: p x destY: p y width: sourceRect width height: sourceRect height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId << 8 + 255));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (sourceRect translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
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

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 5/8/2023 14:43:05'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	(BitBltCanvas onForm: Display) image: ((Form dotOfSize: 50) asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	| f |
	aColor isTransparent ifFalse: [
		BitBltCanvas accessProtect critical: [
			f := BitBltCanvas buildAuxWith: aForm multipliedWith: aColor.
			self image: f at: aPoint sourceRect: aForm boundingBox ]]! !


!BitBltCanvas class methodsFor: 'services' stamp: 'jmv 5/8/2023 14:50:15'!
buildAuxWith: aForm multipliedWith: aColor
	| h w r |
	w := aForm width.
	h := aForm height.
	AuxForm
		ifNotNil: [
			w := w max: AuxForm width.
			h := h max: AuxForm height.
			(AuxForm width < w or: [ AuxForm height < h ])
				ifTrue: [ AuxForm := nil ]
				ifFalse: [ AuxForm bits atAllPut: 0 ]].
	AuxForm
		ifNil: [
			AuxForm := Form extent: w@h depth: 32.
			AuxBlitter := BitBlt toForm: AuxForm ].
	
	r := aForm boundingBox.
	AuxForm fill: r fillColor: aColor.
	AuxBlitter
		sourceForm: aForm;
		combinationRule: Form rgbMul;
		sourceRect: r;
		copyBits.
	^AuxForm! !


!VectorCanvas methodsFor: 'private' stamp: 'jmv 5/8/2023 14:38:19'!
pvtDrawButtonFaceForm: aForm
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
		at: (currentMorph morphExtent - aForm extent //2).! !


!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 5/8/2023 14:25:23'!
image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	(BitBltCanvas onForm: Display) image: ((Form dotOfSize: 50) asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		self class accessProtect critical: [
			self class buildAuxWith: aForm multipliedWith: aColor.
			self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 5/8/2023 14:25:17'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	(BitBltCanvas onForm: Display) fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"
	self class accessProtect critical: [
		self class buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!methodRemoval: BitBltCanvas #buildAuxWith:multipliedWith: stamp: 'jmv 5/8/2023 14:52:10'!
BitBltCanvas removeSelector: #buildAuxWith:multipliedWith:!
