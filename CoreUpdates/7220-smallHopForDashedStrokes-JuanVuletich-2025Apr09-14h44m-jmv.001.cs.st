'From Cuis7.3 [latest update: #7129] on 9 April 2025 at 3:11:37 pm'!

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/9/2025 15:09:32'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		numberOfBitsInSequence is the number of bits in onOffBitSequence
		lengthOfEachBit is exactly that.
		Offset can be used to do 'walking dashes' effects. It is an integer in [0 .. numberOfBitsInSequence-1]."

	| sharper apparentWeight strokeColorToUse strokeWidthToUse hopToUse |
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
	hopToUse := onOffBitSequence > 0
		ifTrue: [ 0.5 ] 								"Don't use larger hop. Always start each dash from the beginning."
		ifFalse: [ aStrokeWidth sqrt / 2.0 max: 0.5 ]. 	"See #experimentHopsPerPixel2."
	self pvtStrokeWidth: strokeWidthToUse hop: hopToUse.
	self pvtStrokeColor: strokeColorToUse.
	self pvtFillColor: nil.

	"For dashed strokes"
	self pvtDashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset.

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/9/2025 14:44:06'!
strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock
	"Prepare parameters for drawing a dashed stroke.
	This spec is periodic, the pattern is repeated as needed.
		- onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		- clientLengthOfEachBit is exactly that, the actual length of stroke for each bit
		- offsetOrNil can be used to do 'walking dashes' effects. It is an integer (nil defaults to 0)."

	| bitSequence bitSequenceLength lengthOfEachBit offset |
	bitSequence := onOffBitSequence asInteger max: 1.
	bitSequenceLength := bitSequence highBit.
	lengthOfEachBit := clientLengthOfEachBit ifNil: [ 1.0 ].
	lengthOfEachBit := lengthOfEachBit asFloat.
	lengthOfEachBit <= 0.0 ifTrue: [ lengthOfEachBit := 1.0 ].
	lengthOfEachBit := geometryTransformation externalizeScalar: lengthOfEachBit.
	offset := offsetOrNil ifNil: [0].
	offset := offset asInteger \\ bitSequenceLength.

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		dashedStrokeBits: bitSequence
		dashBitCount: bitSequence highBit 
		dashBitLength: lengthOfEachBit
		dashBitOffset: offset
		do: pathCommandsBlock
		ensureClosePath: false! !


!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 4/9/2025 15:07:28'!
pvtStrokeWidth: aNumber hop: aFloat

	| swErodedHalf |
	strokeWidth := aNumber.
	hop := aFloat.
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
	swDilatedHalfSquared := swDilatedHalf * swDilatedHalf.
	"swErodedHalf is the inner radious of the alphas ring.
	See #updateAlphasForX:y:"
	swErodedHalf := strokeWidth - antiAliasingWidth * 0.5 -hop - 2.5.
	swErodedHalfSquared := swErodedHalf * swErodedHalf abs.! !

