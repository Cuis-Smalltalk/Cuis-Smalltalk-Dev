'From Cuis7.3 [latest update: #7106] on 1 April 2025 at 9:33:56 am'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight targetAssumedOpaque morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec dashBitOffset dashBitCount dashBitLength dashedStrokeBits '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 18:39:29'!
strokeWidth: strokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: lengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		- onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		- lengthOfEachBit is exactly that, the actual length of stroke for each bit
		- offsetOrNil can be used to do 'walking dashes' effects. It is an integer (nil defaults to 0).
	+ onOffBitSequence is usually stated in base-2.
	A possible example is 2r1110000100000. This draws '---    -     '
	"

	self initializePath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		dashedStrokeBits: onOffBitSequence dashBitLength: lengthOfEachBit dashBitOffset: offsetOrNil
		do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 18:35:59'!
strokeWidth: strokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence do: pathCommandsBlock
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		- onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
			A possible example is 2r1110000100000. This draws '---    -     '
	"

	self strokeWidth: strokeWidth
		color: aStrokeColor
		dashedStrokeBits: onOffBitSequence
		dashBitLength: nil
		dashBitOffset: nil
		do: pathCommandsBlock! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 18:30:14'!
strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock

	^self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 11:58:36'!
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
	self pvtHopLength: 0.5.									"See #experimentHopsPerPixel2"

	"For dashed strokes"
	dashedStrokeBits := onOffBitSequence.
	dashBitCount := numberOfBitsInSequence.
	dashBitLength := lengthOfEachBit.
	dashBitOffset := offset.

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/1/2025 09:13:48'!
strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock
	"Prepare parameters for drawing a dashed stroke. This spec is periodic, the pattern is repeated as needed.
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


!Sample02Bezier methodsFor: 'drawing' stamp: 'jmv 3/28/2025 18:33:07'!
drawOn: aCanvas
	"Two Bezier curves. A Quadratic one (with start, end and a single control point), and a Cubic one (with start, end, and two control points)
	Sample02Bezier new openInHand
	"

	aCanvas strokeWidth: 4 color: Color green dashedStrokeBits: 2r1111000010000 dashBitLength: 2 dashBitOffset: dashOffset do: [
		aCanvas
				moveTo: aStart;
				quadraticBezierTo: aEnd control: aControlPoint ].

	aCanvas strokeWidth: 1 color: Color darkGray dashedStrokeBits: 16rFFFF0000 dashBitLength: 0.3 dashBitOffset: 0 do: [
		aCanvas
				moveTo: aStart;
				lineTo: aControlPoint;
				lineTo: aEnd ].

	aCanvas strokeWidth: 3 color: Color red dashedStrokeBits: 2r1000000010000000 dashBitLength: 2 dashBitOffset: dashOffset do: [
		aCanvas
				moveTo: bStart;
				cubicBezierTo: bEnd control1: bControlPoint1 control2: bControlPoint2 ].
	aCanvas strokeWidth: 1 color: Color darkGray dashedStrokeBits: 2r01010101010101010101010101010101 dashBitLength: 2 dashBitOffset: 0 do: [
		aCanvas
				moveTo: bStart;
				lineTo: bControlPoint1;
				lineTo: bControlPoint2;
				lineTo: bEnd ].! !

!Sample02Bezier methodsFor: 'stepping' stamp: 'jmv 3/28/2025 11:03:23'!
step

	dashOffset := dashOffset + 1.
	self redrawNeeded.! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 11:58:44'!
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
	aStrokeWidth < antiAliasingWidth
		ifTrue: [
			"Strokes are really thin, but strokeColor is same as fillColor, so there's no problem sampling them."
			self pvtHopLength: 0.5 ]								"See #experimentHopsPerPixel1"
		ifFalse: [
			self pvtHopLength: 0.5 ].								"See #experimentHopsPerPixel2."

	"Only used for strokes with no fill"
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 11:58:50'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.
	self pvtHopLength: 0.5.										"See #experimentHopsPerPixel3"

	"Only used for strokes with no fill"
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 3/28/2025 11:55:58'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		dashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0
		do: pathCommandsBlock
		ensureClosePath: false! !


!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/1/2025 09:30:25'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r alphaWord doUpdate dySquared dx dxSquared dy pixelIndex dxp bit |

	"If dashed strokes, only draw if in a dash, not in a gap."
	dashBitLength = 0.0 ifFalse: [
		"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
		trajectoryLength := trajectoryLength + hop.
		"Correct behavior for negative offset given by #floor and #\\"
		bit := (trajectoryLength / dashBitLength + dashBitOffset) floor \\ dashBitCount.
		(dashedStrokeBits bitAt: dashBitCount - bit) = 1 ifFalse: [ ^self ]].

	"Compute affected rect. Honor clipRect"
	t := (y - swDilatedHalf + 1) truncated.		"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf - subPixelDelta + 1) truncated.		"(z+1) truncated works equally well than (z ceiling)"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + swDilatedHalf + subPixelDelta) truncated.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			alphaWord := alphaMask bytesAt: pixelIndex.
			alphaWord = "16r007F7F7F" #[16r7F 16r7F 16r7F 16r00] ifFalse: [
				doUpdate := false.
				dy := displayY - y.
				dySquared := dy * dy.
				dx := displayX - x.
				"Red"
				dxp := dx - subPixelDelta.
				dxSquared := dxp * dxp.
				distanceToAxisSquared := dxSquared + dySquared.
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha := distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > (alphaWord at: 3) ifTrue: [
							alphaWord at: 3 put: alpha.
							doUpdate := true ]].
				"Green"
				dxSquared := dx * dx.
				distanceToAxisSquared := dxSquared + dySquared.
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha := distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > (alphaWord at: 2) ifTrue: [
							alphaWord at: 2 put: alpha.
							doUpdate := true ]].
				"Blue"
				dxp := dx + subPixelDelta.
				dxSquared := dxp * dxp.
				distanceToAxisSquared := dxSquared + dySquared.
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha := distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > (alphaWord at: 1) ifTrue: [
							alphaWord at: 1 put: alpha.
							doUpdate := true ]].
				doUpdate ifTrue: [
					alphaMask bytesAt: pixelIndex put: alphaWord ]]]]! !


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/1/2025 09:30:35'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex bit |

	"If dashed strokes, only draw if in a dash, not in a gap."
	dashBitLength = 0.0 ifFalse: [
		"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
		trajectoryLength := trajectoryLength + hop.
		"Correct behavior for negative offset given by #floor and #\\"
		bit := (trajectoryLength / dashBitLength + dashBitOffset) floor \\ dashBitCount.
		(dashedStrokeBits bitAt: dashBitCount - bit) = 1 ifFalse: [ ^self ]].

	"Compute affected rect. Honor clipRect."
	t := (y - swDilatedHalf + 1) truncated.			"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf + 1) truncated.			"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + swDilatedHalf) truncated.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		dy := displayY - y.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			prevAlpha := alphaMask at: pixelIndex.
			prevAlpha = 16r7F ifFalse: [
				dx := displayX - x.
				distanceToAxisSquared := (dx*dx) + (dy*dy).
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha := distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > prevAlpha ifTrue: [
							alphaMask at: pixelIndex put: alpha ]]]]]! !

!methodRemoval: VectorEngineDrawer #strokeWidth:color:strokeDashArray:strokeDashArrayOffset:do: stamp: 'jmv 4/1/2025 09:25:57'!
VectorEngineDrawer removeSelector: #strokeWidth:color:strokeDashArray:strokeDashArrayOffset:do:!
!methodRemoval: VectorEngineDrawer #basicStrokeWidth:color:strokeDashArray:strokeDashArrayLast:strokeDashArrayOffset:do:ensureClosePath: stamp: 'jmv 4/1/2025 09:26:40'!
VectorEngineDrawer removeSelector: #basicStrokeWidth:color:strokeDashArray:strokeDashArrayLast:strokeDashArrayOffset:do:ensureClosePath:!
!methodRemoval: VectorEngineBoundsFinder #strokeWidth:color:strokeDashArray:strokeDashArrayOffset:do: stamp: 'jmv 4/1/2025 09:25:57'!
VectorEngineBoundsFinder removeSelector: #strokeWidth:color:strokeDashArray:strokeDashArrayOffset:do:!
!methodRemoval: AbstractVectorCanvas #strokeWidth:color:strokeDashArray:strokeDashArrayOffset:do: stamp: 'jmv 4/1/2025 09:25:52'!
AbstractVectorCanvas removeSelector: #strokeWidth:color:strokeDashArray:strokeDashArrayOffset:do:!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight targetAssumedOpaque morphIds edgeCounts alphaMask contour strokeColor dashedStrokeBits dashBitCount dashBitLength dashBitOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
