'From Cuis7.3 [latest update: #7118] on 8 April 2025 at 12:35:56 pm'!

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 4/8/2025 12:34:38'!
initForPath
	"Perform any initialization needed before starting a Path"

	firstPoint := nil.
	currentPoint := `0.0@0.0`.
	lastControlPoint := currentPoint.! !


!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 4/8/2025 12:34:42'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	self initForPath.
	engine fillColor: aFillColor do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 4/8/2025 12:34:47'!
strokeWidth: strokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: lengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		- onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		- lengthOfEachBit is exactly that, the actual length of stroke for each bit
		- offsetOrNil can be used to do 'walking dashes' effects. It is an integer (nil defaults to 0).
	+ onOffBitSequence is usually stated in base-2.
	A possible example is 2r1110000100000. This draws '---    -     '
	"

	self initForPath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		dashedStrokeBits: onOffBitSequence dashBitLength: lengthOfEachBit dashBitOffset: offsetOrNil
		do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 4/8/2025 12:34:51'!
strokeWidth: strokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self initForPath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 4/8/2025 12:34:56'!
strokeWidth: strokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	self initForPath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		fillColor: aFillColor
		do: pathCommandsBlock.! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/8/2025 12:35:07'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	| hopToUse |
	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	aStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [
		^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor
			dashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0
			do: pathCommandsBlock ensureClosePath: false ].

	"If stroke is thinner than antiAliasingWidth, we need to make it wider and less opaque.
	But this requires doing fill first and the blending stroke over it, or else fill gets eroded by widened stroke.
	(this is only relevant if fill ~= stroke~)"
	(aStrokeWidth < antiAliasingWidth and: [ aStrokeColor ~= aFillColor ]) ifTrue: [
		self fillColor: aFillColor do: pathCommandsBlock.
		canvas ifNotNil: [ canvas initForPath ].
		^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor
			dashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0
			do: pathCommandsBlock ensureClosePath: true ].

	hopToUse := aStrokeWidth < 4
		ifTrue: [ 0.5 ] "See #experimentHopsPerPixel1"
		ifFalse: [ 1.0 ]. "See #experimentHopsPerPixel2."
				"Correct edgeCount for fills requires visiting at least once every y coordinate, hop can not be larger than 1."
	self pvtStrokeWidth: aStrokeWidth hop: hopToUse.
	self pvtStrokeColor: aStrokeColor.
	self pvtFillColor: aFillColor.

	"Only used for strokes with no fill"
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!methodRemoval: AbstractVectorCanvas #initializePath stamp: 'jmv 4/8/2025 12:35:37'!
AbstractVectorCanvas removeSelector: #initializePath!
