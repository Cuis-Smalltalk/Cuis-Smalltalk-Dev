'From Cuis7.1 [latest update: #6777] on 16 October 2024 at 9:53:21 am'!

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 10/16/2024 09:52:10'!
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
	srcBox := 0@0 corner: srcForm extent-1.
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

