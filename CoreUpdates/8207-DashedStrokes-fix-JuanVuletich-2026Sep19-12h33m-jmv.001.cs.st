'From Cuis7.9 [latest update: #8206] on 19 September 2026 at 12:40:13 pm'!

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 19/Sep/2026, 3:34:01 pm (UTC)'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		numberOfBitsInSequence is the number of bits in onOffBitSequence
		lengthOfEachBit is exactly that.
		Offset can be used to do 'walking dashes' effects. It is an integer in [0 .. numberOfBitsInSequence-1]."

	| sharper apparentWeight strokeColorToUse strokeWidthToUse |
	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].

	strokeWidthToUse := aStrokeWidth.
	strokeColorToUse := aStrokeColor.
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
				strokeColorToUse := aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / apparentWeight ]
			ifFalse: [
				"If the requested stroke width is too thin, make them wide enough to be properly sampled."
				"{'(wider)desired weight'. aStrokeWidth. 'but would get'. antiAliasingWidth. 'correction'. aStrokeWidth / antiAliasingWidth} print."
				strokeWidthToUse := antiAliasingWidth.
				strokeColorToUse := aStrokeColor alpha: aStrokeColor alpha * aStrokeWidth / antiAliasingWidth ]].
	self pvtStrokeWidth: strokeWidthToUse strokeColor: strokeColorToUse fillColor: nil.
	self pvtDashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset.
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 19/Sep/2026, 3:37:56 pm (UTC)'!
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
		canvas ifNotNil: [ canvas initForPath ].
		^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor
			dashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0
			do: pathCommandsBlock ensureClosePath: true ].

	self pvtStrokeWidth: aStrokeWidth strokeColor: aStrokeColor fillColor: aFillColor.
	self pvtDashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0.
	self doPath: pathCommandsBlock ensureClosePath: true.
! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 19/Sep/2026, 3:38:28 pm (UTC)'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].

	self pvtStrokeWidth: 0.0 strokeColor: nil fillColor: aFillColor.
	self pvtDashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0.
	self doPath: pathCommandsBlock ensureClosePath: true.
! !

