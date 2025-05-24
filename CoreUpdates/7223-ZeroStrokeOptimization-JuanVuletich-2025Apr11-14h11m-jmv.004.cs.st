'From Cuis7.3 [latest update: #7130] on 11 April 2025 at 2:48:44 pm'!

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/11/2025 14:30:15'!
updateAlphasZeroStrokeForX: x y: y
	"This optimization is currently only implemented for the Whole Pixel engine.
	See implementors and senders."

	self updateAlphasForX: x y: y! !


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/11/2025 14:40:11'!
updateAlphasZeroStrokeForX: x y: y
	"Optimization.
	Assumptions:
		- strokeWidth = 0.0
		- antiAliasingWidth = 1.6 (Default for whole pixel anti aliasing)
	Therefore:
		- swDilatedHalf = 0.8
		- swDilatedHalfSquared = 0.64
		- aawScaledInverse = 79.375
		- No use for needsFullAlphaCircle, swErodedHalfSquared
		- No support for dashedStrokes
		"
	| distanceToAxisSquared alpha t b l r prevAlpha dx dxSquared dx2 dx2Squared dy dySquared dy2 dy2Squared pixelIndex affectedBitsIndex1 affectedBitsIndex2 |
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
	affectedBitsIndex1 := -1.
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
				affectedBitsIndex1 := pixelIndex-1>>4+1.
				(affectedBits at: affectedBitsIndex1) = 0 ifTrue: [
					affectedBits at: affectedBitsIndex1 put: 1 ].
				alphaMask at: pixelIndex put: alpha ]].
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
					affectedBitsIndex2 := pixelIndex+1-1>>4+1.
					affectedBitsIndex2 = affectedBitsIndex1 ifFalse: [
						(affectedBits at: affectedBitsIndex2) = 0 ifTrue: [
							affectedBits at: affectedBitsIndex2 put: 1 ]].
					alphaMask at: pixelIndex+1 put: alpha ]].
			distanceToAxisSquared < 0.36 ifTrue: [ ^self ]]].

	t = b ifTrue: [ ^self ].

	pixelIndex := b * targetWidth + l + 1.
	affectedBitsIndex1 := -1.
	dy2 := dy + 1.
	dy2Squared := dy2 * dy2.
	distanceToAxisSquared := dxSquared + dy2Squared.
	distanceToAxisSquared < 0.64 ifTrue: [
		prevAlpha := alphaMask at: pixelIndex.
		prevAlpha = 16r7F ifFalse: [
			alpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) truncated.
			alpha > prevAlpha ifTrue: [
				affectedBitsIndex1 := pixelIndex-1>>4+1.
				(affectedBits at: affectedBitsIndex1) = 0 ifTrue: [
					affectedBits at: affectedBitsIndex1 put: 1 ].
				alphaMask at: pixelIndex put: alpha ]].
		distanceToAxisSquared < 0.36 ifTrue: [ ^self ]].

	r = l ifFalse: [
		distanceToAxisSquared := dx2Squared + dy2Squared.
		distanceToAxisSquared < 0.64 ifTrue: [
			prevAlpha := alphaMask at: pixelIndex+1.
			prevAlpha = 16r7F ifFalse: [
				alpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) truncated.
				alpha > prevAlpha ifTrue: [
					affectedBitsIndex2 := pixelIndex+1-1>>4+1.
					affectedBitsIndex2 = affectedBitsIndex1 ifFalse: [
						(affectedBits at: affectedBitsIndex2) = 0 ifTrue: [
							affectedBits at: affectedBitsIndex2 put: 1 ]].
					alphaMask at: pixelIndex+1 put: alpha ]]]].! !


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/11/2025 14:30:26'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex bit lastUpdated affectedBitsIndex |

	"Use this optimized varsion if possible."	
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
							alpha := ((swDilatedHalf - distanceToAxisSquared sqrt min: antiAliasingWidth) * aawScaledInverse) truncated.
							alpha > prevAlpha ifTrue: [
								affectedBitsIndex := pixelIndex-1>>4+1.
								lastUpdated = affectedBitsIndex ifFalse: [ "Slight optimization"
									(affectedBits at: affectedBitsIndex) = 1 ifFalse: [
										affectedBits at: affectedBitsIndex put: 1.
										lastUpdated := affectedBitsIndex ]].
								alphaMask at: pixelIndex put: alpha ]]]]]].
	needsFullAlphaCircle := false.! !

