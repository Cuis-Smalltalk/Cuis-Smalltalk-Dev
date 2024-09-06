'From Cuis7.1 [latest update: #6676] on 6 September 2024 at 7:33:54 pm'!

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 9/6/2024 19:31:44'!
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
				fillBitmap: (Bitmap with: (currentMorph morphId << 8 + 255));
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

