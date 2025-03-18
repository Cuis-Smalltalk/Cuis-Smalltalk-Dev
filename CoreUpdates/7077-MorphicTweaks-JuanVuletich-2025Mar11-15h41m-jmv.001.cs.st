'From Cuis7.3 [latest update: #7070] on 11 March 2025 at 3:41:27 pm'!

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/11/2025 15:20:20'!
image: srcForm at: aPoint
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
	BitBlt is enough, but we need two of them!!"
	(world notNil and: [currentMorph == world]) ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: 0; sourceY: 0;
			destX: p x destY: p y width: srcForm width height: srcForm height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId <<8 + 255));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
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

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/11/2025 15:20:27'!
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
	BitBlt is enough, but we need two of them!!"
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
				fillBitmap: (Bitmap with: (currentMorph morphId <<8 + 255));
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


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 3/11/2025 15:19:47'!
morphIdAt: aPoint
	| index |
	index := aPoint y * targetWidth + aPoint x + 1.
	(index between: 1 and: morphIds size) ifFalse: [ ^0 ].
	^(morphIds at: index) >>8! !


!VectorEngineSmalltalk methodsFor: 'accessing' stamp: 'jmv 3/11/2025 15:20:38'!
clipCurrentMorph: aBoolean
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex := displayY * targetWidth + clipLeft.
			clipLeft to: clipRight do: [ :displayX |
				pixelIndex := pixelIndex + 1.
				morphIds at: pixelIndex put: ((morphIds at: pixelIndex) >>8 <<8) ]]].
	clipCurrentMorph := aBoolean.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms - aux' stamp: 'jmv 3/7/2025 15:00:32'!
updateContourForX: x y: y
	"The Contour of the current morph is the pixel area affected, but without holes.
	For each y line where a pixel is affected, record the leftmost and rightmost affected pixels.
	Note: Only includes pen trajectory, but not stroke width. Ignores any clipping."

	| thisYRounded |
	thisYRounded := y rounded.
	(thisYRounded between: 0 and: targetHeight-1) ifTrue: [
		thisYRounded = prevYRounded ifFalse: [
			prevYRounded = 16r3FFFFFFF ifFalse: [
				contour at: prevYRounded * 2 + 1 put: leftAtThisY.
				contour at: prevYRounded * 2 + 2 put: rightAtThisY ].
			leftAtThisY := contour at: thisYRounded * 2 + 1.
			rightAtThisY := contour at: thisYRounded * 2 + 2.
			prevYRounded := thisYRounded ].
		leftAtThisY := leftAtThisY min: x.
		rightAtThisY := rightAtThisY max: x ].! !

