Class {
	#name : 'VectorEngineDrawer',
	#superclass : 'VectorEngine',
	#instVars : [
		'targetForm',
		'targetWidth',
		'targetHeight',
		'targetAssumedOpaque',
		'morphIds',
		'edgeCounts',
		'alphaMask',
		'affectedBits',
		'contour',
		'strokeColor',
		'dashedStrokeBits',
		'dashBitCount',
		'dashBitLength',
		'dashBitOffset',
		'fillColor',
		'clipLeft',
		'clipTop',
		'clipRight',
		'clipBottom',
		'currentMorphId',
		'clippingSpec'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'private' }
VectorEngineDrawer >> auxPixelDataClass [
	^self subclassResponsibility
]

{ #category : 'stroke & fill' }
VectorEngineDrawer >> basicStrokeWidth: aStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset do: pathCommandsBlock ensureClosePath: ensureClosePath [
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

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.
]

{ #category : 'stroke & fill' }
VectorEngineDrawer >> basicStrokeWidth: aStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock [
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
		ifFalse: [ 0.9 ]. "See #experimentHopsPerPixel2."
				"Correct edgeCount for fills requires visiting at least once every y coordinate, hop can
				not be larger than 1. 0.9 gives a security margin for hop estimation in Bezier curves."
	self pvtStrokeWidth: aStrokeWidth hop: hopToUse.
	self pvtStrokeColor: aStrokeColor.
	self pvtFillColor: aFillColor.

	"Only used for strokes with no fill"
	self pvtDashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0.

	self doPath: pathCommandsBlock ensureClosePath: true.
]

{ #category : 'private' }
VectorEngineDrawer >> clearArrays [
	morphIds atAllPut: 0.
	edgeCounts atAllPut: 0.
	alphaMask atAllPut: 0.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
]

{ #category : 'accessing' }
VectorEngineDrawer >> clipRect: aRectangle  [
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipLeft := aRectangle left asInteger max: 0.
	clipTop := aRectangle top asInteger max: 0.
	clipRight := (aRectangle right asInteger min: targetWidth) - 1.
	clipBottom := (aRectangle bottom asInteger min: targetHeight) - 1
]

{ #category : 'accessing' }
VectorEngineDrawer >> clippingSpec: aClippingSpecOrNil [
	clippingSpec := aClippingSpecOrNil.
]

{ #category : 'accessing' }
VectorEngineDrawer >> currentMorph: aMorph [
	aMorph
		ifNil: [
			currentMorphId := 0 ]
		ifNotNil: [
			currentMorphId := aMorph morphId ].
]

{ #category : 'accessing' }
VectorEngineDrawer >> disableSubPixelSampling [
	"NOP here.
	To be redefined if needed"
]

{ #category : 'accessing' }
VectorEngineDrawer >> disableTargetTranslucency [
	"Avoid the cost of computing correct target transucency.
	This is the default behavior. Target translucency is not required when drawing to Display for live Morphic.
	See #enableTargetTranslucency."

	targetAssumedOpaque := true
]

{ #category : 'accessing' }
VectorEngineDrawer >> enableTargetTranslucency [
	"Compute correct target transucency.
	This is usually not needed, and it is computationally more expensive. Default is #disableTargetTranslucency.
	It is required, for instance, for #imageForm and similar methods, so result is transparent outside of the drawing."

	targetAssumedOpaque := false
]

{ #category : 'stroke & fill' }
VectorEngineDrawer >> fillColor: aFillColor do: pathCommandsBlock [
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0 hop: 0.5. "See #experimentHopsPerPixel3"
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.

	"Only used for strokes with no fill"
	self pvtDashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0.

	self doPath: pathCommandsBlock ensureClosePath: true.
]

{ #category : 'private' }
VectorEngineDrawer >> initializeOnForm: aForm [
	self currentMorph: nil.
	self pvtSetForm: aForm.
]

{ #category : 'services' }
VectorEngineDrawer >> isConsistentWithTarget [
	"and display!!!!!"
	targetForm isNil ifTrue: [ ^false ].
	targetForm extent = Display extent ifFalse: [ ^false ].
	^edgeCounts size = (targetWidth * targetHeight) and: [
		targetWidth = targetForm width and: [ targetForm height = targetHeight ]].
]

{ #category : 'accessing' }
VectorEngineDrawer >> morphContourAfterDrawInterectInto: aGlobalContour [
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or answer a new array if not.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment.
	See #morphContourAfterDrawInto:into:
	See #morphContourAfterDrawUpdate:oldTop:oldBottom:into:
	"

	| y0 y1 dilationHalfHeight strokeHalfSquared |
	y0 := morphBoundsTop max: 0.
	y1 := morphBoundsBottom min: targetHeight -1.
	dilationHalfHeight := (strokeWidth / 2.0) rounded.
	strokeHalfSquared := (strokeWidth / 2.0) squared.
	y0 to: y1 do: [ :y | | x0 x1 i |
		x0 := targetWidth.
		x1 := 0.
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y + dy between: y0 and: y1) ifTrue: [
				dx := (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 := x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 := x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		i := y*2+1.
		aGlobalContour at: i put: ((aGlobalContour at: i) max: x0).
		aGlobalContour at: i+1 put: ((aGlobalContour at: i+1) min: x1) ].
]

{ #category : 'accessing' }
VectorEngineDrawer >> morphContourAfterDrawInto: contourArrayOrNil into: aBlock [
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or answer a new array if not.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment.
	See #morphContourAfterDrawUpdate:oldTop:oldBottom:into:
	See #morphContourAfterDrawInterectInto:
	"

	| morphContour i requiredSize y0 y1 dilationHalfHeight strokeHalfSquared |
	y0 := morphBoundsTop max: 0.
	y1 := morphBoundsBottom min: targetHeight -1.
	requiredSize := (y1 - y0 + 1) *  2 max: 0.
	morphContour := contourArrayOrNil.
	(morphContour isNil or: [ requiredSize > morphContour size ]) ifTrue: [
		morphContour := IntegerArray new: requiredSize ].
	dilationHalfHeight := (strokeWidth / 2.0) rounded.
	strokeHalfSquared := (strokeWidth / 2.0) squared.
	i := 1.
	y0 to: y1 do: [ :y | | x0 x1 |
		x0 := targetWidth.
		x1 := 0.
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y + dy between: y0 and: y1) ifTrue: [
				dx := (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 := x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 := x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		morphContour at: i put: x0.
		morphContour at: i+1 put: x1.
		i := i + 2 ].
	i to: morphContour size by: 2 do: [ :ii |
		morphContour at: ii put: targetWidth.
		morphContour at: ii+1 put: 0 ].
	aBlock value: morphContour value: y0 value: y1.
]

{ #category : 'accessing' }
VectorEngineDrawer >> morphContourAfterDrawUpdate: oldContour oldTop: oldTop oldBottom: oldBottom into: aBlock [
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment.
	See #morphContourAfterDrawInto:into:
	See #morphContourAfterDrawInterectInto:
	"

	| morphContour i ii requiredSize oldContourDelta y0 y1 dilationHalfHeight strokeHalfSquared mbb mbt |
	y0 := (morphBoundsTop min: oldTop) max: 0.
	y1 := (morphBoundsBottom max: oldBottom) min: targetHeight -1.
	requiredSize := (y1 - y0 + 1) *  2.
	morphContour := oldContour.
	oldContourDelta := 0.
	(morphContour size < requiredSize or: [y0 < oldTop] ) ifTrue: [
		oldContourDelta := (oldTop - y0) * 2.
		morphContour := IntegerArray new: requiredSize ].
	dilationHalfHeight := (strokeWidth / 2.0) rounded.
	strokeHalfSquared := (strokeWidth / 2.0) squared.
	i := 1.
	ii := 1-oldContourDelta.
	mbt := morphBoundsTop max: 0.
	mbb := morphBoundsBottom min: targetHeight -1.
	y0 to: y1 do: [ :y | | x0 x1 |
		(ii between: 1 and: oldContour size-1) ifTrue: [
			x0 := oldContour at: ii.
			x1 := oldContour at: ii+1 ]
		ifFalse: [
			x0 := targetWidth.
			x1 := 0 ].
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y+dy between: mbt and: mbb) ifTrue: [
				dx := (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 := x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 := x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		morphContour at: i put: x0.
		morphContour at: i+1 put: x1.
		i := i + 2.
		ii := ii + 2 ].
	i to: morphContour size by: 2 do: [ :iii |
		morphContour at: iii put: targetWidth.
		morphContour at: iii+1 put: 0 ].
	aBlock value: morphContour value: y0 value: y1.
]

{ #category : 'private' }
VectorEngineDrawer >> morphIdAt: aPoint [
	| index |
	index := aPoint y * targetWidth + aPoint x + 1.
	(index between: 1 and: morphIds size) ifFalse: [ ^0 ].
	^morphIds at: index
]

{ #category : 'accessing' }
VectorEngineDrawer >> morphIds [
	^morphIds
]

{ #category : 'services' }
VectorEngineDrawer >> pinArrays [
	"Pin if pinning is supported by the VM, NOP otherwise."
	"Warning: #pin may fail for Spur images if run on the SqueakJS VM, at least on October 2023."

	targetForm bits pin.
	morphIds pin.
	edgeCounts pin.
	alphaMask pin.
	affectedBits pin.
	contour pin.
]

{ #category : 'private' }
VectorEngineDrawer >> pvtDashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset [
	"For dashed strokes"

	dashedStrokeBits := onOffBitSequence.
	dashBitCount := numberOfBitsInSequence.
	dashBitLength := lengthOfEachBit.
	dashBitOffset := offset.
]

{ #category : 'private' }
VectorEngineDrawer >> pvtSetForm: aForm [

	| s |
	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	targetAssumedOpaque := true. 		"Set to false for true translucency on target."
	s := targetWidth * targetHeight.
	morphIds := WordArray new: s.
	edgeCounts := self auxPixelDataClass new: s.
	alphaMask := self auxPixelDataClass new: s.
	affectedBits := ByteArray new: s+15 // 16.
	contour := Float32Array new: targetHeight * 2.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.
]

{ #category : 'private' }
VectorEngineDrawer >> resetMorphBounds [

	morphBoundsTop := targetHeight - 1.
	morphBoundsBottom := 0.
	morphBoundsLeft := targetWidth - 1.
	morphBoundsRight := 0
]

{ #category : 'stroke & fill' }
VectorEngineDrawer >> strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock [
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
		ensureClosePath: false
]

{ #category : 'stroke & fill' }
VectorEngineDrawer >> strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		dashedStrokeBits: 0 dashBitCount: 0 dashBitLength: 0.0 dashBitOffset: 0
		do: pathCommandsBlock
		ensureClosePath: false
]

{ #category : 'stroke & fill' }
VectorEngineDrawer >> strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke, fill, or both."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		fillColor: aFillColor
		do: pathCommandsBlock
]

{ #category : 'accessing' }
VectorEngineDrawer >> subPixelDelta [
	"To be redefined if needed"

	^0.0
]

{ #category : 'accessing' }
VectorEngineDrawer >> target [
	^targetForm
]

{ #category : 'services' }
VectorEngineDrawer >> unpinAndFreeMemory [

	"Unpin, if possible, any pinned large arrays."
	"Unpin if pinning is supported by the VM, NOP otherwise."
	targetForm ifNotNil: [
		(DisplayScreen isDisplay: targetForm) ifFalse: [
			targetForm bits unpin ]].
	morphIds ifNotNil: [ morphIds unpin ].
	edgeCounts ifNotNil: [ edgeCounts unpin ].
	alphaMask ifNotNil: [ alphaMask unpin ].
	affectedBits ifNotNil: [ affectedBits unpin ].
	contour ifNotNil: [ contour unpin ].

	targetForm := nil.
	morphIds := nil.
	edgeCounts := nil.
	alphaMask := nil.
	affectedBits := nil.
	contour := nil.
]

{ #category : 'testing' }
VectorEngineDrawer >> usesVectorEnginePlugin [
	^false
]
