'From Cuis7.5 [latest update: #7732] on 28 November 2025 at 1:15:08 pm'!

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'len 28/Nov/2025 13:13:00'!
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
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'len 28/Nov/2025 13:10:00'!
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
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'len 28/Nov/2025 13:11:00'!
imageUnrotated: srcForm at: aPoint
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
! !

