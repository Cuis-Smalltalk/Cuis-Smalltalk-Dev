'From Cuis7.3 [latest update: #7113] on 6 April 2025 at 1:48:51 pm'!

!VectorEngine class methodsFor: 'experiments' stamp: 'jmv 4/6/2025 13:47:42'!
experimentHopsPerPixel2
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 2: Stroke of various widths, no fill
	VectorEngine experimentHopsPerPixel2
	"
	"In the following script, err is the error admitted. It is consistent with a hop of 0.5 for fills."
	"
	| err r rads hop hopx |
	'------' print.
	err := 0.25.
	#(0.001 0.01 0.1 0.2 0.25 0.3 0.5 1 1.4 10 100 200 265 400 800 1600 3200 6400 12800) do: [ :w |
		r := w / 2.
		rads := (1-(err/r)) arcCos.
		hop := rads sin * r. 'theoretical'.
		hopx := w sqrt / 2.0 max: 0.5. 'approximated'.
		{r. hop. hopx} print ].
	"
	| d engine f1 f2 hops1 hops2 strokeWidth |
	hops1 := 200.
	hops1 := 20.
	hops2 := 3.
	strokeWidth := 1.4.
	f1 := Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine := self onForm: f1 subPixelAntiAliasing: true.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 50@30;
			lineTo: 190@135; lineTo: 50 @50;
			lineTo: 135@390; lineTo: 30@50;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 := Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine := self onForm: f2 subPixelAntiAliasing: true.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 50@30;
			lineTo: 190@135; lineTo: 50@50;
			lineTo: 135@390; lineTo: 30@50;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 4/6/2025 13:33:35'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].
	self pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth).
	self pvtHopLength: (aStrokeWidth sqrt / 2.0 max: 0.5). "See #experimentHopsPerPixel2"
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/6/2025 13:48:03'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		numberOfBitsInSequence is the number of bits in onOffBitSequence
		lengthOfEachBit is exactly that.
		Offset can be used to do 'walking dashes' effects. It is an integer in [0 .. numberOfBitsInSequence-1]."

	| sharper apparentWeight |
	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"There are two possible strategies when requested stroke width is narrower than anti aliasing filter.
			To more effectively avoid aliasing, set sharper to false.
			To draw sharper thin lines, at the price of very little extra aliasing in them, set sharper to true.
			This could be turned into a Preference."
			sharper := true.
			sharper ifTrue: [
				"If the requested stroke width is used, filter will be clipped, resulting in an incorrect weight for the stroke."
				apparentWeight := ((aStrokeWidth squared / antiAliasingWidth) + (2.0 * aStrokeWidth) + antiAliasingWidth) / 4.0.
				"{'(sharper)desired weight'. aStrokeWidth. 'but would get'. apparentWeight. 'correction'. aStrokeWidth / apparentWeight} print."
				self pvtStrokeWidth: aStrokeWidth.
				self pvtStrokeColor: (aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / apparentWeight)]
			ifFalse: [
				"If the requested stroke width is too thin, make them wide enough to be properly sampled."
				"{'(wider)desired weight'. aStrokeWidth. 'but would get'. antiAliasingWidth. 'correction'. aStrokeWidth / antiAliasingWidth} print."
				self pvtStrokeWidth: antiAliasingWidth.
				self pvtStrokeColor: (aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / antiAliasingWidth)]
			]
		ifFalse: [
			self pvtStrokeWidth: aStrokeWidth.
			self pvtStrokeColor: aStrokeColor ].
	self pvtFillColor: nil.
	self pvtHopLength: (aStrokeWidth sqrt / 2.0 max: 0.5).  "See #experimentHopsPerPixel2"

	"For dashed strokes"
	dashedStrokeBits := onOffBitSequence.
	dashBitCount := numberOfBitsInSequence.
	dashBitLength := lengthOfEachBit.
	dashBitOffset := offset.

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/6/2025 13:43:47'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

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
		canvas ifNotNil: [ canvas initializePath ].
		^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor
			dashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0
			do: pathCommandsBlock ensureClosePath: true ].

	self pvtStrokeWidth: aStrokeWidth.
	self pvtStrokeColor: aStrokeColor.
	self pvtFillColor: aFillColor.
	aStrokeWidth < 4
		ifTrue: [ "See #experimentHopsPerPixel1"
			self pvtHopLength: 0.5 ]
		ifFalse: [ "See #experimentHopsPerPixel2."
			"Correct edgeCount for fills requires visiting at least once every y coordinate, hop can not be larger than 1."
			self pvtHopLength: 1.0 ].	

	"Only used for strokes with no fill"
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/6/2025 13:43:59'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.
	self pvtHopLength: 0.5.		"See #experimentHopsPerPixel3"

	"Only used for strokes with no fill"
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

