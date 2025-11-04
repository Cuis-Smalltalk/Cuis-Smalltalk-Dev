'From Cuis7.5 [latest update: #7682] on 3 November 2025 at 12:23:43 pm'!

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 11/3/2025 10:54:34'!
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
		dashBitCount: bitSequenceLength
		dashBitLength: lengthOfEachBit
		dashBitOffset: offset
		do: pathCommandsBlock
		ensureClosePath: false! !

