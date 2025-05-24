'From Cuis7.3 [latest update: #7114] on 7 April 2025 at 11:25:45 am'!
!classDefinition: #VectorEngineSmalltalk category: #'Morphic-Support'!
VectorEngineDrawer subclass: #VectorEngineSmalltalk
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY aawScaledInverse swDilatedHalf swDilatedHalfSquared swErodedHalfSquared trajectoryLength needsFullAlphaCircle '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!VectorEngine methodsFor: 'private' stamp: 'jmv 4/7/2025 10:47:53'!
pvtStrokeWidth: aNumber hop: aFloat

	strokeWidth := aNumber.
	hop := aFloat.! !


!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 4/7/2025 11:04:07'!
pvtStrokeWidth: aNumber hop: aFloat

	strokeWidth := aNumber.
	hop := aFloat.
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.! !


!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 4/7/2025 11:11:52'!
pvtStrokeWidth: aNumber hop: aFloat

	| swErodedHalf |
	strokeWidth := aNumber.
	hop := aFloat.
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
	swDilatedHalfSquared := swDilatedHalf * swDilatedHalf.
	"swErodedHalf is the inner radious of the alphas ring.
	See #updateAlphasForX:y:"
	swErodedHalf := strokeWidth - antiAliasingWidth * 0.5 -hop - 2.
	swErodedHalfSquared := swErodedHalf * swErodedHalf abs.! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/7/2025 11:04:22'!
pvtStrokeWidth: aNumber hop: aFloat

	strokeWidth := aNumber.
	hop := aFloat.
	self primStrokeWidth: strokeWidth hop: hop.! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 4/7/2025 11:02:56'!
primAntiAliasingWidth: aNumber subPixelDelta: otherFloat
	<primitive: 'primAntiAliasingWidthsubPixelDelta' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 4/7/2025 11:00:15'!
primStrokeWidth: aNumber hop: aFloat
	<primitive: 'primStrokeWidthHop' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 4/7/2025 11:13:02'!
basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath
	"Prepare parameters for drawing stroke."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].
	self
		pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth)
		hop: (aStrokeWidth sqrt / 2.0 max: 0.5). "See #experimentHopsPerPixel2"
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 4/7/2025 11:13:28'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0 hop: 0.5.	"See #experimentHopsPerPixel3"
	self doPath: pathCommandsBlock ensureClosePath: true.! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/7/2025 11:24:26'!
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
	self pvtStrokeWidth: strokeWidthToUse hop: (aStrokeWidth sqrt / 2.0 max: 0.5).  "See #experimentHopsPerPixel2"
	self pvtStrokeColor: strokeColorToUse.
	self pvtFillColor: nil.

	"For dashed strokes"
	dashedStrokeBits := onOffBitSequence.
	dashBitCount := numberOfBitsInSequence.
	dashBitLength := lengthOfEachBit.
	dashBitOffset := offset.

	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/7/2025 11:17:41'!
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
		canvas ifNotNil: [ canvas initializePath ].
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

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 4/7/2025 11:18:07'!
fillColor: aFillColor do: pathCommandsBlock
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0 hop: 0.5. "See #experimentHopsPerPixel3"
	self pvtStrokeColor: nil.
	self pvtFillColor: aFillColor.

	"Only used for strokes with no fill"
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.

	self doPath: pathCommandsBlock ensureClosePath: true.! !


!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 4/6/2025 15:38:40'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	| dx dy trx try tcx tcy center angle xp yp x y d hops |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	dx := geometryTransformation externalizeDelta: rx@0.
	dy := geometryTransformation externalizeDelta: 0@ry.
	trx := dx r.
	try := dy r.
	hops := ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d := hops asFloat.
	center := geometryTransformation externalizePosition: cx@cy.
	tcx := center x.
	tcy := center y.
	0 to: hops do: [ :h |
		angle := h / d * sweepAngle + startAngle.
		xp := angle cos * trx.
		yp := angle sin * try.
		x := thetaCos * xp - (thetaSin * yp) + tcx.
		y := thetaSin * xp + (thetaCos * yp) + tcy.
		spanLeft := spanLeft min: x.
		spanTop := spanTop min: y.
		spanRight := spanRight max: x.
		spanBottom := spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y. ].! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 4/6/2025 15:38:35'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	| p txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy hops xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 f23 f4 x y |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl1@yControl1. txControl1 := p x. tyControl1 := p y.
	p := geometryTransformation externalizePosition: xControl2@yControl2. txControl2 := p x. tyControl2 := p y.
	"Factors found by trial and error. Seems to honor hopsPerPixel in the worst case."
	dx := ((txControl1-txFrom) abs max: (txTo-txControl2) abs)*3 max: (txControl2-txControl1) abs*1.5.
	dy := ((tyControl1-tyFrom) abs max: (tyTo-tyControl2) abs)*3 max: (tyControl2-tyControl1) abs*1.5.
	hops := ((dx max: dy) / hop) truncated + 1.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight := spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop := spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom := spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).

	t := 0.0.
	increment := 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT.
		f23 := 3.0 * oneLessT * t.
		f2 := f23 * oneLessT.
		f3 := f23 * t.
		f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 4/6/2025 15:38:30'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| p txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	dx := (txTo - txFrom) abs.
	dy := (tyTo - tyFrom) abs.
	hops := ((dx max: dy) / hop) truncated + 1.

	spanLeft := spanLeft min: (txFrom min: txTo).
	spanRight := spanRight max: (txFrom max: txTo).
	spanTop := spanTop min: (tyFrom min: tyTo).
	spanBottom := spanBottom max: (tyFrom max: tyTo).

	t := 0.0.
	increment := 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		x := (oneLessT * txFrom) + (t * txTo).
		y := (oneLessT * tyFrom) + (t * tyTo).
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 4/6/2025 15:38:24'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	| p txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 x y t0 x0 y0 length correction |

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.

	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl@yControl. txControl := p x. tyControl := p y.

	dx := (txTo-txFrom) abs.
	dx2 := (txControl-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	dy2 := (tyControl-tyFrom) abs.

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+txControl) / 2.0).
	spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+txControl) / 2.0).
	spanTop := spanTop min: (yMinEnd min: (yMinEnd+tyControl) / 2.0).
	spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+tyControl) / 2.0).

	"Case t = 0.0"
	x := txFrom.
	y := tyFrom.
	self updateAlphasForX: x y: y.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].
	self updateContourForX: x y: y.

	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx max: dy) min: 0.5.
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
		x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		length := ((x-x0)@(y-y0)) r.
		correction := hop / length.
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			length := ((x-x0)@(y-y0)) r.
			correction := hop / length.
			correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 4/6/2025 15:38:47'!
newTrajectoryFragment

	prevYTruncated := 16r3FFFFFFF.! !


!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/7/2025 11:11:28'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r alphaWord doUpdate dySquared dx dy pixelIndex dxp bit lastUpdated affectedBitsIndex |
	
	"In a basic implementation, this method computes anti aliasing translucency for all possibly affected pixels as determined by
	the outer radius of the pen nib: swDilatedHalfSquared.
	But usually the next point visited is very close to the last, and there's no real need to fill the inner part of the pen nib as opaque,
	as it was already set. So, unless needsFullAlphaCircle, only the outer ring is computed and stored.
	The inner radius of this ring is swErodedHalfSquared"

	"If dashed strokes, only draw if in a dash, not in a gap."
	dashBitLength = 0.0 ifFalse: [
		"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
		trajectoryLength := trajectoryLength + hop.
		"This code is robust against #floor and #\\ (and their translation to C, and the C compiler) because dashBitOffset is guaranteed to be non negative."
		bit := (trajectoryLength / dashBitLength + dashBitOffset) floor \\ dashBitCount.
		(dashedStrokeBits bitAt: dashBitCount - bit) = 1 ifFalse: [
			needsFullAlphaCircle := true.
			^self ]].

	"Compute affected rect. Honor clipRect"
	t := (y - swDilatedHalf + 1) truncated.		"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf - subPixelDelta + 1) truncated.		"(z+1) truncated works equally well than (z ceiling)"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + swDilatedHalf + subPixelDelta) truncated.
	r > clipRight ifTrue: [ r := clipRight ].

	lastUpdated := -1.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			dx := displayX - x.
			distanceToAxisSquared := dx * dx + dySquared. "Use Green subpixel for this."
			(needsFullAlphaCircle or: [distanceToAxisSquared > swErodedHalfSquared]) ifTrue: [
				alphaWord := alphaMask bytesAt: pixelIndex.
				alphaWord = "16r007F7F7F" #[16r7F 16r7F 16r7F 16r00] ifFalse: [
					doUpdate := false.
					"Red"
					dxp := dx - subPixelDelta.
					distanceToAxisSquared := dxp * dxp + dySquared.
					distanceToAxisSquared < swDilatedHalfSquared
						ifTrue: [
							alpha := (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated.
							alpha > (alphaWord at: 3) ifTrue: [
								alphaWord at: 3 put: (alpha min: 16r7F).
								doUpdate := true ]].
					"Green"
					distanceToAxisSquared := dx * dx + dySquared.
					distanceToAxisSquared < swDilatedHalfSquared
						ifTrue: [
							alpha := (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated.
							alpha > (alphaWord at: 2) ifTrue: [
								alphaWord at: 2 put: (alpha min: 16r7F).
								doUpdate := true ]].
					"Blue"
					dxp := dx + subPixelDelta.
					distanceToAxisSquared := dxp * dxp + dySquared.
					distanceToAxisSquared < swDilatedHalfSquared
						ifTrue: [
							alpha := (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated.
							alpha > (alphaWord at: 1) ifTrue: [
								alphaWord at: 1 put: (alpha min: 16r7F).
								doUpdate := true ]].
					doUpdate ifTrue: [
						affectedBitsIndex := pixelIndex-1>>4+1.
						lastUpdated = affectedBitsIndex ifFalse: [ "Slight optimization"
							(affectedBits at: affectedBitsIndex) = 1 ifFalse: [
								affectedBits at: affectedBitsIndex put: 1.
								lastUpdated := affectedBitsIndex ]].
						alphaMask bytesAt: pixelIndex put: alphaWord ]]]]].
	needsFullAlphaCircle := false.! !


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/6/2025 15:29:49'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex bit lastUpdated affectedBitsIndex |
	
	"In a basic implementation, this method computes anti aliasing translucency for all possibly affected pixels as determined by
	the outer radius of the pen nib: swDilatedHalfSquared.
	But usually the next point visited is very close to the last, and there's no real need to fill the inner part of the pen nib as opaque,
	as it was already set. So, unless needsFullAlphaCircle, only the outer ring is computed and stored.
	The inner radius of this ring is swErodedHalfSquared"

	"If dashed strokes, only draw if in a dash, not in a gap."
	dashBitLength = 0.0 ifFalse: [
		"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
		trajectoryLength := trajectoryLength + hop.
		"This code is robust against #floor and #\\ (and their translation to C, and the C compiler) because dashBitOffset is guaranteed to be non negative."
		bit := (trajectoryLength / dashBitLength + dashBitOffset) floor \\ dashBitCount.
		(dashedStrokeBits bitAt: dashBitCount - bit) = 1 ifFalse: [
			needsFullAlphaCircle := true.
			^self ]].

	"Compute affected rect. Honor clipRect."
	t := (y - swDilatedHalf + 1) truncated.			"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf + 1) truncated.			"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + swDilatedHalf) truncated.
	r > clipRight ifTrue: [ r := clipRight ].

	lastUpdated := -1.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		dy := displayY - y.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			dx := displayX - x.
			distanceToAxisSquared := (dx*dx) + (dy*dy).
			(needsFullAlphaCircle or: [distanceToAxisSquared > swErodedHalfSquared]) ifTrue: [
				prevAlpha := alphaMask at: pixelIndex.
				prevAlpha = 16r7F ifFalse: [
					distanceToAxisSquared < swDilatedHalfSquared
						ifTrue: [
							alpha := (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated.
							alpha > prevAlpha ifTrue: [
								affectedBitsIndex := pixelIndex-1>>4+1.
								lastUpdated = affectedBitsIndex ifFalse: [ "Slight optimization"
									(affectedBits at: affectedBitsIndex) = 1 ifFalse: [
										affectedBits at: affectedBitsIndex put: 1.
										lastUpdated := affectedBitsIndex ]].
								alphaMask at: pixelIndex put: (alpha min: 16r7F) ]]]]]].
	needsFullAlphaCircle := false.! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/7/2025 11:03:07'!
initializePath

	super initializePath.

	"Set all plugin state. There might be several instances of this class, all using the same plugin.
	Only one of them will use the plugin during a path draw operation. See class variable AccessLock."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self primTargetAssumedOpaque: targetAssumedOpaque.
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat.
	self primClipLeft: clipLeft clipTop: clipTop clipRight: clipRight clipBottom: clipBottom.
	self primCurrentMorphId: currentMorphId.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.
	self primStrokeWidth: strokeWidth hop: hop asFloat.
	self primStrokeColor: strokeColor.
	self primFillColor: fillColor.

	self primInitializePath.! !

!methodRemoval: VectorEngineWithPlugin #primStrokeWidth: stamp: 'jmv 4/7/2025 11:24:55'!
VectorEngineWithPlugin removeSelector: #primStrokeWidth:!
!methodRemoval: VectorEngineWithPlugin #primAntiAliasingWidth:subPixelDelta:hopLength: stamp: 'jmv 4/7/2025 11:03:20'!
VectorEngineWithPlugin removeSelector: #primAntiAliasingWidth:subPixelDelta:hopLength:!
!methodRemoval: VectorEngineWithPlugin #pvtStrokeWidth: stamp: 'jmv 4/7/2025 11:24:39'!
VectorEngineWithPlugin removeSelector: #pvtStrokeWidth:!
!methodRemoval: VectorEngineSmalltalk #pvtStrokeWidth: stamp: 'jmv 4/7/2025 11:24:38'!
VectorEngineSmalltalk removeSelector: #pvtStrokeWidth:!
!methodRemoval: VectorEngineBoundsFinder #pvtStrokeWidth: stamp: 'jmv 4/7/2025 11:24:36'!
VectorEngineBoundsFinder removeSelector: #pvtStrokeWidth:!
!methodRemoval: VectorEngine #pvtHopLength: stamp: 'jmv 4/7/2025 11:24:50'!
VectorEngine removeSelector: #pvtHopLength:!
!methodRemoval: VectorEngine #pvtStrokeWidth: stamp: 'jmv 4/7/2025 11:24:43'!
VectorEngine removeSelector: #pvtStrokeWidth:!
!classDefinition: #VectorEngineSmalltalk category: #'Morphic-Support'!
VectorEngineDrawer subclass: #VectorEngineSmalltalk
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY aawScaledInverse swDilatedHalf swDilatedHalfSquared swErodedHalfSquared trajectoryLength needsFullAlphaCircle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
