'From Cuis7.3 [latest update: #7127] on 9 April 2025 at 11:40:30 am'!

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/9/2025 11:39:19'!
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


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/9/2025 11:39:31'!
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

