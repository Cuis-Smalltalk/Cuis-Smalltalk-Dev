'From Cuis6.3 [latest update: #6326] on 25 April 2024 at 10:50:34 am'!

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
				"At least two dashes and gaps specified. Are we in a gap?"
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
				"At least two dashes and gaps specified. Are we in a gap?"
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

