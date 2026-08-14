'From Cuis7.9 [latest update: #8137] on 13 August 2026 at 11:40:44 am'!

!VectorEngine methodsFor: 'aux' stamp: 'jmv 12/Aug/2026, 10:31:47 pm (UTC)'!
hop
	"See #experimentHopsPerPixel2
	See #experimentHopsPerPixel3"

	^((strokeWidth+0.7) sqrt *0.38) + (strokeWidth / 80.0)! !


!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:30:42 pm (UTC)'!
pvtStrokeWidth: aNumber
	
	strokeWidth := aNumber.
	hop := self hop. "Will only be used for ellipses and arcs (not for full circles or any other shape)"
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
! !

!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:32:21 pm (UTC)'!
pvtStrokeWidth: aNumber strokeColor: aStrokeColorOrNil fillColor: aFillColorOrNil
	
	self pvtStrokeWidth: aNumber.
! !


!VectorEngineDrawer methodsFor: 'aux' stamp: 'jmv 12/Aug/2026, 10:45:28 pm (UTC)'!
hop
	| answer |

	answer := super hop.

	"If doing dashed strokes, don't use larger hop. Always start each dash from the beginning."
	dashedStrokeBits = 0 ifFalse: [
		answer := answer min: 0.5 ].

	"Correct edgeCount for fills requires visiting at least once every y coordinate, hop can
	not be larger than 1. 0.9 gives a security margin for hop estimation in Bezier curves."
	fillColor notNil ifTrue: [
		answer := answer min: 0.9 ].

	^answer! !


!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:30:51 pm (UTC)'!
pvtStrokeWidth: aNumber
	| swErodedHalf |
	strokeWidth := aNumber.
	hop := self hop.
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
	swDilatedHalfSquared := swDilatedHalf * swDilatedHalf.
	"swErodedHalf is the inner radious of the alphas ring.
	See #updateAlphasForX:y:"
	swErodedHalf := strokeWidth - antiAliasingWidth * 0.5 -hop - 2.5.
	swErodedHalfSquared := swErodedHalf * swErodedHalf abs.
! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:34:05 pm (UTC)'!
pvtStrokeWidth: aNumber strokeColor: aStrokeColorOrNil fillColor: aFillColorOrNil

	strokeColor := aStrokeColorOrNil.
	fillColor := aFillColorOrNil.
	self pvtStrokeWidth: aNumber.! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:31:05 pm (UTC)'!
pvtStrokeWidth: aNumber
	strokeWidth := aNumber.
	hop := self hop.
	"Set AntiAliasingWidth in the plugin, as #primStrokeWidth:hop: needs it."
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat.
	"This call in the plugin uses AntiAliasingWith."
	self primStrokeWidth: strokeWidth hop: hop.
! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:34:12 pm (UTC)'!
pvtStrokeWidth: aNumber strokeColor: aStrokeColorOrNil fillColor: aFillColorOrNil
	strokeColor := aStrokeColorOrNil.
	fillColor := aFillColorOrNil.
	self pvtStrokeWidth: aNumber.
	strokeColor notNil
		ifTrue: [
			self
				primStrokeR: strokeColor red
				g: strokeColor green
				b: strokeColor blue
				a: strokeColor alpha]
		ifFalse: [self primStrokeR: 0.0 g: 0.0 b: 0.0 a: 0.0].
	fillColor notNil
		ifTrue: [
			self
				primFillR: fillColor red
				g: fillColor green
				b: fillColor blue
				a: fillColor alpha]
		ifFalse: [self primFillR: 0.0 g: 0.0 b: 0.0 a: 0.0].
! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 10:31:03 pm (UTC)'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].

	self pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth) strokeColor: aStrokeColor fillColor: nil.
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.
! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 10:31:09 pm (UTC)'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].

	self pvtStrokeWidth: 0.0 strokeColor: nil fillColor: aFillColor.
	self doPath: pathCommandsBlock ensureClosePath: true.
! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 10:30:51 pm (UTC)'!
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

	"For dashed strokes"
	self pvtDashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset.

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
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 10:30:29 pm (UTC)'!
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

	"Only used for strokes with no fill"
	self pvtDashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0.

	self pvtStrokeWidth: aStrokeWidth strokeColor: aStrokeColor fillColor: aFillColor.
	self doPath: pathCommandsBlock ensureClosePath: true.
! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 10:31:24 pm (UTC)'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].

	"Only used for strokes with no fill"
	self pvtDashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0.

	self pvtStrokeWidth: 0.0 strokeColor: nil fillColor: aFillColor.
	self doPath: pathCommandsBlock ensureClosePath: true.
! !

!methodRemoval: VectorEngineWithPlugin #pvtFillColor: stamp: 'jmv 13/Aug/2026, 2:39:39 pm (UTC)'!
VectorEngineWithPlugin removeSelector: #pvtFillColor:!
!methodRemoval: VectorEngineWithPlugin #pvtStrokeColor: stamp: 'jmv 13/Aug/2026, 2:39:39 pm (UTC)'!
VectorEngineWithPlugin removeSelector: #pvtStrokeColor:!
!methodRemoval: VectorEngineWithPlugin #pvtStrokeWidth:hop: stamp: 'jmv 13/Aug/2026, 2:39:39 pm (UTC)'!
VectorEngineWithPlugin removeSelector: #pvtStrokeWidth:hop:!
!methodRemoval: VectorEngineSmalltalk #pvtFillColor: stamp: 'jmv 13/Aug/2026, 2:39:40 pm (UTC)'!
VectorEngineSmalltalk removeSelector: #pvtFillColor:!
!methodRemoval: VectorEngineSmalltalk #pvtStrokeColor: stamp: 'jmv 13/Aug/2026, 2:39:40 pm (UTC)'!
VectorEngineSmalltalk removeSelector: #pvtStrokeColor:!
!methodRemoval: VectorEngineSmalltalk #pvtStrokeWidth:hop: stamp: 'jmv 13/Aug/2026, 2:39:40 pm (UTC)'!
VectorEngineSmalltalk removeSelector: #pvtStrokeWidth:hop:!
!methodRemoval: VectorEngineDrawer #pvtStrokeColor: stamp: 'jmv 13/Aug/2026, 2:39:40 pm (UTC)'!
VectorEngineDrawer removeSelector: #pvtStrokeColor:!
!methodRemoval: VectorEngineDrawer #pvtFillColor: stamp: 'jmv 13/Aug/2026, 2:39:40 pm (UTC)'!
VectorEngineDrawer removeSelector: #pvtFillColor:!
!methodRemoval: VectorEngineBoundsFinder #pvtStrokeWidth:hop: stamp: 'jmv 13/Aug/2026, 2:39:41 pm (UTC)'!
VectorEngineBoundsFinder removeSelector: #pvtStrokeWidth:hop:!
!methodRemoval: VectorEngine #pvtStrokeWidth:hop: stamp: 'jmv 13/Aug/2026, 2:39:42 pm (UTC)'!
VectorEngine removeSelector: #pvtStrokeWidth:hop:!
