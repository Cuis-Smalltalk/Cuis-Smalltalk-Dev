'From Cuis7.3 [latest update: #7111] on 3 April 2025 at 3:38:51 pm'!

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/3/2025 13:32:18'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r alphaWord doUpdate dySquared dx dxSquared dy pixelIndex dxp bit lastUpdated affectedBitsIndex |

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
lastUpdated := -1.
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
					
					"ESTO"
					"solo pixelIndex>>4 en slang"
					affectedBitsIndex := pixelIndex-1>>4+1.
					lastUpdated = affectedBitsIndex
						ifFalse: [
							"Esta pobre optimizacion baja a la 3ra parte.
							Lo óptimo sería la 1ma parte!!"
							(affectedBits at: affectedBitsIndex) = 1
								ifFalse: [
									affectedBits at: affectedBitsIndex put: 1.
									lastUpdated := affectedBitsIndex ]].
							"esto obviamente reescribe todavia muchas veces, de llamados anteriores.
							optimizar si es posible."
					
					alphaMask bytesAt: pixelIndex put: alphaWord ]]]]! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/3/2025 14:40:44'!
updateEdgeCountAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills.
		
	See https://en.wikipedia.org/wiki/Nonzero-rule .

Where it says 'pick a ray in any direction', we pick an horizontal ray pointing to the left. This means that for any point (actually for every pixel) p1=(x1, y1), we record any crossings of y1, i.e. any sequence of path points (... pp1, pp2, .....) where the jump from pp1 to pp2 crosses y1, and pp1.x and pp2.x are at the left of x1. If the path is going up, it is clockwwise. If it is going down, it is counter clockwise.

As an additional optimization, as we need to store for every pixel all the crossings to the left, what we actually store are the crossings at each pixel (i.e. within 1 pixel distance to the left). Then, when we use them, the algorithms iterates from left to right, and accumulates the counters from previous pixels.
	"

	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex increment countWord redCount greenCount blueCount redOffset greenOffset blueOffset affectedBitsIndex affectedBitsIndex2 |

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
				(affectedBits at: affectedBitsIndex) = 1 ifFalse: [
					affectedBits at: affectedBitsIndex put: 1 ] ].
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
				(affectedBits at: affectedBitsIndex) = 1 ifFalse: [
					affectedBits at: affectedBitsIndex put: 1 ].
				affectedBitsIndex2 := bluePixelIndex-1>>4+1.
				affectedBitsIndex2 = affectedBitsIndex ifFalse: [
					(affectedBits at: affectedBitsIndex2) = 1 ifFalse: [
						affectedBits at: affectedBitsIndex2 put: 1 ]].
		].! !


!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/3/2025 13:32:31'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex bit lastUpdated affectedBitsIndex |

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

lastUpdated := -1.
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
							
							"ESTO"
							"solo pixelIndex>>4 en slang"
							affectedBitsIndex := pixelIndex-1>>4+1.
							lastUpdated = affectedBitsIndex
								ifFalse: [
									"Esta pobre optimizacion baja a la 3ra parte.
									Lo óptimo sería la 1ma parte!!"
									(affectedBits at: affectedBitsIndex) = 1
										ifFalse: [
											affectedBits at: affectedBitsIndex put: 1.
											lastUpdated := affectedBitsIndex ]].
									"esto obviamente reescribe todavia muchas veces, de llamados anteriores.
									optimizar si es posible."
							
							alphaMask at: pixelIndex put: alpha ]]]]]! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 4/3/2025 14:37:12'!
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
		(affectedBits at: affectedBitsIndex) = 1 ifFalse: [
			affectedBits at: affectedBitsIndex put: 1 ]].
	prevYTruncated := thisYTruncated.! !

