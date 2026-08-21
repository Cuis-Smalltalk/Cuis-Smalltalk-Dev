'From Cuis7.9 [latest update: #8141] on 19 August 2026 at 10:22:53 am'!

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 18/Aug/2026, 4:09:54 pm (UTC)'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| p txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.

	spanLeft := spanLeft min: (txFrom min: txTo).
	spanRight := spanRight max: (txFrom max: txTo).
	spanTop := spanTop min: (tyFrom min: tyTo).
	spanBottom := spanBottom max: (tyFrom max: tyTo).
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	dx := (txTo - txFrom) abs.
	dy := (tyTo - tyFrom) abs.
	hops := ((dx max: dy) / hop) truncated + 1.
	increment := 1.0 / hops.
	t := 0.0.
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
	self updateContourForX: txTo y: tyTo.
! !


!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 18/Aug/2026, 3:34:52 pm (UTC)'!
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
		bit := (trajectoryLength / dashBitLength) floor + dashBitOffset \\ dashBitCount.
		(dashedStrokeBits bitAt: dashBitCount - bit) = 1 ifFalse: [
			needsFullAlphaCircle := true.
			^self ]].

	"Compute affected rect. Honor clipRect"
	t := (y - swDilatedHalf + 1) truncated.						"(z+1) truncated works equally well than the more intuitive (z ceiling)"
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
						alphaMask bytesAt: pixelIndex put: alphaWord.
						affectedBitsIndex := pixelIndex-1>>4+1.
						affectedBitsIndex = lastUpdated ifFalse: [
							affectedBits at: affectedBitsIndex put: 1.
							lastUpdated := affectedBitsIndex ]]]]]].
	needsFullAlphaCircle := false.! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 18/Aug/2026, 3:23:59 pm (UTC)'!
updateEdgeCountAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills.
		
	See https://en.wikipedia.org/wiki/Nonzero-rule .

Where it says 'pick a ray in any direction', we pick an horizontal ray pointing to the left. This means that for any point (actually for every pixel) p1=(x1, y1), we record any crossings of y1, i.e. any sequence of path points (... pp1, pp2, .....) where the jump from pp1 to pp2 crosses y1, and pp1.x and pp2.x are at the left of x1. If the path is going up, it is clockwwise. If it is going down, it is counter clockwise.

As an additional optimization, as we need to store for every pixel all the crossings to the left, what we actually store are the crossings at each pixel (i.e. within 1 pixel distance to the left). Then, when we use them, the algorithms iterates from left to right, and accumulates the counters from previous pixels.
	"

	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex increment countWord redCount greenCount blueCount redOffset greenOffset blueOffset affectedBitsIndex |

	thisYTruncated := y truncated.
	thisYTruncated = prevYTruncated ifTrue: [
		^ self].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ self ].
	prevYTruncated = 16r3FFFFFFF ifTrue: [ 
		^ prevYTruncated := thisYTruncated ].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY := thisYTruncated.
			increment := 1 ]
		ifFalse: [
			pixelY := prevYTruncated.
			increment := -1 ].
	prevYTruncated := thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelIndexBase := pixelY * targetWidth.
	redOffset := (x+subPixelDelta+1) truncated max: clipLeft.			"take the next red subpixel center to the right of x"
	greenOffset := (x+1) truncated max: clipLeft.											"take the next green subpixel center to the right of x"
	blueOffset := (x-subPixelDelta+1) truncated max: clipLeft.		"take the next blue subpixel center to the right of x"
	redPixelIndex := pixelIndexBase + redOffset +1.
	greenPixelIndex := pixelIndexBase + greenOffset +1.
	bluePixelIndex := pixelIndexBase + blueOffset +1.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord := edgeCounts bytesAt: redPixelIndex.
				redCount := (countWord at: 3) + increment bitAnd: 16rFF.
				countWord at: 3 put: redCount.
				greenCount := (countWord at: 2) + increment bitAnd: 16rFF.
				countWord at: 2 put: greenCount.
				blueCount := (countWord at: 1) + increment bitAnd: 16rFF.
				countWord at: 1 put: blueCount.
				edgeCounts bytesAt: redPixelIndex put: countWord.
				affectedBitsIndex := redPixelIndex-1>>4+1.
				affectedBits at: affectedBitsIndex put: 1 ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in previous"
					redOffset <= clipRight ifTrue: [
						countWord := edgeCounts bytesAt: redPixelIndex.
						redCount := (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						greenCount := (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						edgeCounts bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord := edgeCounts bytesAt: bluePixelIndex.
						blueCount := (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCounts bytesAt: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the previous"
					redOffset <= clipRight ifTrue: [
						countWord := edgeCounts bytesAt: redPixelIndex.
						redCount := (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						edgeCounts bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord := edgeCounts bytesAt: bluePixelIndex.
						greenCount := (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						blueCount := (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCounts bytesAt: bluePixelIndex put: countWord ].
				].
				affectedBitsIndex := redPixelIndex-1>>4+1.
				affectedBits at: affectedBitsIndex put: 1.
				affectedBitsIndex := bluePixelIndex-1>>4+1.
				affectedBits at: affectedBitsIndex put: 1.
		].! !


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 18/Aug/2026, 3:04:46 pm (UTC)'!
updateAlphasForX: x y: y
	| distanceToAxisSquared distanceToOuterEdgeOfAA alpha t b l r prevAlpha dx dy dySquared pixelIndex bit affectedBitsIndex nextPixelIndexToUpdateAffectedBits |

	"Use this optimized version if possible. It is always used, for instance, for TrueType."	
	(strokeWidth = 0.0 and: [antiAliasingWidth = 1.6]) ifTrue: [
		^self updateAlphasZeroStrokeForX: x y: y ].

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
		bit := (trajectoryLength / dashBitLength) floor + dashBitOffset \\ dashBitCount.
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

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		nextPixelIndexToUpdateAffectedBits := pixelIndex+1.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			dx := displayX - x.
			distanceToAxisSquared := dx*dx + dySquared.
			distanceToAxisSquared < swDilatedHalfSquared
				ifTrue: [
					(needsFullAlphaCircle or: [distanceToAxisSquared > swErodedHalfSquared]) ifTrue: [
						prevAlpha := alphaMask at: pixelIndex.
						prevAlpha = 16r7F ifFalse: [
							alpha := 127.
							distanceToOuterEdgeOfAA := swDilatedHalf - distanceToAxisSquared sqrt.
							distanceToOuterEdgeOfAA < antiAliasingWidth
								ifTrue: [alpha := (distanceToOuterEdgeOfAA  * aawScaledInverse) truncated].
							alpha > prevAlpha ifTrue: [
								alphaMask at: pixelIndex put: alpha.
								pixelIndex >= nextPixelIndexToUpdateAffectedBits ifTrue: [
									affectedBitsIndex := pixelIndex-1>>4+1.
									affectedBits at: affectedBitsIndex put: 1.
									"nextPixelIndexToUpdateAffectedBits := affectedBitsIndex+1 -1 << 4 + 1." "Both have same effect."
									nextPixelIndexToUpdateAffectedBits := (pixelIndex-1 bitAnd: 16rFFFFFFF0) + 16r10 + 1 ]]]]]]].
	needsFullAlphaCircle := false.
! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 18/Aug/2026, 3:10:42 pm (UTC)'!
updateAlphasZeroStrokeForX: x y: y
	"Optimization.
	Assumptions:
		- strokeWidth = 0.0
		- antiAliasingWidth = 1.6 (Default for whole pixel anti aliasing)
		- No support for dashedStrokes
	Therefore:
		- swDilatedHalf = 0.8
		- swDilatedHalfSquared = 0.64
		- aawScaledInverse = 79.375
		- No use for needsFullAlphaCircle, swErodedHalfSquared
		- 4 pixels are affected (maybe less, if clipping)
		"
	| distanceToAxisSquared alpha t b l r prevAlpha dx dxSquared dx2 dx2Squared dy dySquared dy2 dy2Squared pixelIndex affectedBitsIndex |
	"Compute affected rect. Honor clipRect."
	t := y truncated.
	b := t + 1.
	l := x truncated.
	r := l + 1.
	t < clipTop ifTrue: [ t := clipTop ].
	b > clipBottom ifTrue: [ b := clipBottom ].
	l < clipLeft ifTrue: [ l := clipLeft ].
	r > clipRight ifTrue: [ r := clipRight ].
	t> b ifTrue: [ ^self ].
	l > r ifTrue: [ ^self ].

	pixelIndex := t * targetWidth + l + 1.
	dy := t - y.
	dySquared := dy * dy.
	dx := l - x.
	dxSquared := dx * dx.
	distanceToAxisSquared := dxSquared + dySquared.
	distanceToAxisSquared < 0.64 ifTrue: [
		prevAlpha := alphaMask at: pixelIndex.
		prevAlpha = 16r7F ifFalse: [
			alpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) truncated.
			alpha > prevAlpha ifTrue: [
				alphaMask at: pixelIndex put: alpha.
				affectedBitsIndex := pixelIndex-1>>4+1.
				affectedBits at: affectedBitsIndex put: 1 ]].
		distanceToAxisSquared < 0.36 ifTrue: [ ^self ]].

	r = l ifFalse: [
		dx2 := dx + 1.
		dx2Squared := dx2 * dx2.
		distanceToAxisSquared := dx2Squared + dySquared.
		distanceToAxisSquared < 0.64 ifTrue: [
			prevAlpha := alphaMask at: pixelIndex+1.
			prevAlpha = 16r7F ifFalse: [
				alpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) truncated.
				alpha > prevAlpha ifTrue: [
					alphaMask at: pixelIndex+1 put: alpha.
					affectedBitsIndex := pixelIndex+1-1>>4+1.
					affectedBits at: affectedBitsIndex put: 1 ]].
			distanceToAxisSquared < 0.36 ifTrue: [ ^self ]]].

	t = b ifTrue: [ ^self ].

	pixelIndex := b * targetWidth + l + 1.
	dy2 := dy + 1.
	dy2Squared := dy2 * dy2.
	distanceToAxisSquared := dxSquared + dy2Squared.
	distanceToAxisSquared < 0.64 ifTrue: [
		prevAlpha := alphaMask at: pixelIndex.
		prevAlpha = 16r7F ifFalse: [
			alpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) truncated.
			alpha > prevAlpha ifTrue: [
				alphaMask at: pixelIndex put: alpha.
				affectedBitsIndex := pixelIndex-1>>4+1.
				affectedBits at: affectedBitsIndex put: 1 ]].
		distanceToAxisSquared < 0.36 ifTrue: [ ^self ]].

	r = l ifFalse: [
		distanceToAxisSquared := dx2Squared + dy2Squared.
		distanceToAxisSquared < 0.64 ifTrue: [
			prevAlpha := alphaMask at: pixelIndex+1.
			prevAlpha = 16r7F ifFalse: [
				alpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) truncated.
				alpha > prevAlpha ifTrue: [
					alphaMask at: pixelIndex+1 put: alpha.
					affectedBitsIndex := pixelIndex+1-1>>4+1.
					affectedBits at: affectedBitsIndex put: 1 ]]]].! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 18/Aug/2026, 3:21:33 pm (UTC)'!
updateEdgeCountAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills.
		
	See https://en.wikipedia.org/wiki/Nonzero-rule .

Where it says 'pick a ray in any direction', we pick an horizontal ray pointing to the left. This means that for any point (actually for every pixel) p1=(x1, y1), we record any crossings of y1, i.e. any sequence of path points (... pp1, pp2, .....) where the jump from pp1 to pp2 crosses y1, and pp1.x and pp2.x are at the left of x1. If the path is going up, it is clockwwise. If it is going down, it is counter clockwise.

As an additional optimization, as we need to store for every pixel all the crossings to the left, what we actually store are the crossings at each pixel (i.e. within 1 pixel distance to the left). Then, when we use them, the algorithms iterates from left to right, and accumulates the counters from previous pixels.
	"

	| thisYTruncated pixelY pixelIndex increment count pixelX affectedBitsIndex |

	thisYTruncated := y truncated.
	thisYTruncated = prevYTruncated ifTrue: [
		^ self].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ self ].
	prevYTruncated = 16r3FFFFFFF ifTrue: [ 
		^ prevYTruncated := thisYTruncated ].

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelX := (x+1) truncated max: clipLeft.		"take the next pixel center to the right of x"
	pixelX <= clipRight ifTrue: [
		pixelY := thisYTruncated max: prevYTruncated.
		increment := (thisYTruncated - prevYTruncated) sign.
		pixelIndex := pixelY * targetWidth + pixelX +1.
		count := edgeCounts at: pixelIndex.
		count := count + increment bitAnd: 16rFF.
		edgeCounts at: pixelIndex put: count.
		affectedBitsIndex := pixelIndex-1>>4+1.
		affectedBits at: affectedBitsIndex put: 1 ].
	prevYTruncated := thisYTruncated.! !

