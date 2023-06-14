'From Cuis 6.0 [latest update: #5852] on 14 June 2023 at 2:19:21 pm'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor fillColor clipLeft clipTop clipRight clipBottom currentMorphId currentClipsSubmorphs clipCurrentMorph strokeDashArray strokeDashArrayOffset strokeDashLength strokeDashArrayLast '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineSmalltalk category: #'Morphic-Support'!
VectorEngineDrawer subclass: #VectorEngineSmalltalk
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY aawScaledInverse swDilatedHalf swDilatedHalfSquared swErodedHalfSquared trajectoryLength '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 09:54:56'!
strokeWidth: strokeWidth color: aStrokeColor strokeDashArray: sda strokeDashArrayOffset: sdao do: pathCommandsBlock
	"Prepare parameters for drawing stroke.
	Optionally include parameter for dashed strokes."

	self initializePath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		strokeDashArray: sda strokeDashArrayOffset: sdao 
		do: pathCommandsBlock.! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 15:20:16'!
strokeWidth: clientStrokeWidth color: aStrokeColor strokeDashArray: clientSda strokeDashArrayOffset: clientSdao do: pathCommandsBlock
	^self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 12:32:32'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor strokeDashArray: sda strokeDashArrayLast: sdal strokeDashArrayOffset: sdao do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke.
	sda is #(dashEnd gapEnd dashEnd gapEnd ...)
	sdao is simply added as an offset"

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

	"strokeDashLength is not zero only if there is one dash length followed by one dash gap (i.e. the simplest possible dash array)"
	strokeDashLength := (sda notNil and: [ sda size = 2 ])
		ifTrue: [ sda first ]
		ifFalse: [ 0.0 ].
	strokeDashArray := sda.
	strokeDashArrayLast := sdal.
	strokeDashArrayOffset := sdao.

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 6/1/2023 11:01:54'!
strokeWidth: clientStrokeWidth color: aStrokeColor strokeDashArray: clientSda strokeDashArrayOffset: clientSdao do: pathCommandsBlock
	"Prepare parameters for drawing stroke.
	sda must be of even size (or nil). Meaning is #(dashLen gapLen dashLen gapLen ...)
	sdao is simply added as an offset"

	| sda sdao last |
	sda := nil.
	last := 0.0.
	sdao := 0.0.
	(clientSda notNil and: [clientSda size even]) ifTrue: [
		"Parameter is #(dashLen gapLen dashLen gapLen ...)
		convert to #(dashEnd gapEnd dashEnd gapEnd ...)"
		sda := clientSda withIndexCollect: [ :each :i |
				last := last + each.
				 geometryTransformation externalizeScalar: last].
		sdao := geometryTransformation externalizeScalar: clientSdao ].
	last := geometryTransformation externalizeScalar: last.
		
	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		strokeDashArray: sda strokeDashArrayLast: last strokeDashArrayOffset: sdao
		do: pathCommandsBlock
		ensureClosePath: false! !


!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 09:54:13'!
strokeWidth: strokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self initializePath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		do: pathCommandsBlock.! !

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 12:01:38'!
strokeWidth: strokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	self initializePath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		fillColor: aFillColor
		do: pathCommandsBlock.! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 14:58:39'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke, fill, or both."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	aStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [
		^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor
			strokeDashArray: nil strokeDashArrayLast: 0.0 strokeDashArrayOffset: 0.0
			do: pathCommandsBlock ensureClosePath: false ].

	"If stroke is thinner than antiAliasingWidth, we need to make it wider and less opaque.
	But this requires doing fill first and the blending stroke over it, or else fill gets eroded by widened stroke.
	(this is only relevant if fill ~= stroke~)"
	(aStrokeWidth < antiAliasingWidth and: [ aStrokeColor ~= aFillColor ]) ifTrue: [
		self fillColor: aFillColor do: pathCommandsBlock.
		canvas ifNotNil: [ canvas initializePath ].
		^ self basicStrokeWidth: aStrokeWidth color: aStrokeColor
			strokeDashArray: nil strokeDashArrayLast: 0.0 strokeDashArrayOffset: 0.0
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
	strokeDashLength := 0.0.
	strokeDashArray := nil.
	strokeDashArrayLast := 0.0.
	strokeDashArrayOffset := 0.0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 14:58:32'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0.
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.
	self pvtHopLength: 0.5.										"See #experimentHopsPerPixel3"

	"Only used for strokes with no fill"
	strokeDashLength := 0.0.
	strokeDashArray := nil.
	strokeDashArrayLast := 0.0.
	strokeDashArrayOffset := 0.0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 5/30/2023 12:45:51'!
strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
	"Prepare parameters for drawing stroke."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		strokeDashArray: nil strokeDashArrayLast: 0.0 strokeDashArrayOffset: 0.0
		do: pathCommandsBlock
		ensureClosePath: false! !


!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 5/30/2023 12:05:53'!
newTrajectoryFragment

	prevYTruncated := 16r3FFFFFFF.
	trajectoryLength := 0.0.! !


!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 5/30/2023 15:26:03'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r alphaWord doUpdate dySquared dx dxSquared dy pixelIndex rest dxp |

	"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
	trajectoryLength := trajectoryLength + hop.
	"If dashed strokes, only draw if in the dash, not in the gap."
	strokeDashArrayLast = 0.0 ifFalse: [
		rest := trajectoryLength + strokeDashArrayOffset \\ strokeDashArrayLast.
		strokeDashLength = 0.0
			ifFalse: [
				"Only one dash and one gap specified. Are we in the gap?"
				rest > strokeDashLength ifTrue: [ ^self ]]
			ifTrue: [
				"At least two dahes and gaps specified. Are we in a gap?"
				(strokeDashArray findLast: [ :each | rest > each ]) odd ifTrue: [ ^self ]]].

	"Compute affected rect. Honor clipRect"
	t := (y - swDilatedHalf + 1) truncated.													"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf - subPixelDelta + 1) truncated.												"(z+1) truncated works equally well than the more intuitive (z ceiling)"
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


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 5/30/2023 12:40:39'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex rest |

	"Compute trajectory length. If dashed strokes, only draw if in the dash, not in the gap."
	trajectoryLength := trajectoryLength + hop.
	strokeDashArrayLast = 0.0 ifFalse: [
		rest := trajectoryLength + strokeDashArrayOffset \\ strokeDashArrayLast.
		strokeDashLength = 0.0
			ifFalse: [
				"Only one dash and one gap specified. Are we in the gap?"
				rest > strokeDashLength ifTrue: [ ^self ]]
			ifTrue: [
				"At least two dahes and gaps specified. Are we in a gap?"
				(strokeDashArray findLast: [ :each | rest > each ]) odd ifTrue: [ ^self ]]].

	"Compute affected rect. Honor clipRect."
	t := (y - swDilatedHalf + 1) truncated.												"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf + 1) truncated.												"(z+1) truncated works equally well than the more intuitive (z ceiling)"
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

!methodRemoval: VectorEngineDrawer #basicStrokeWidth:color:do:ensureClosePath: stamp: 'jmv 6/14/2023 14:16:43'!
VectorEngineDrawer removeSelector: #basicStrokeWidth:color:do:ensureClosePath:!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId currentClipsSubmorphs clipCurrentMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #VectorEngineSmalltalk category: #'Morphic-Support'!
VectorEngineDrawer subclass: #VectorEngineSmalltalk
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY aawScaledInverse swDilatedHalf swDilatedHalfSquared swErodedHalfSquared trajectoryLength'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
