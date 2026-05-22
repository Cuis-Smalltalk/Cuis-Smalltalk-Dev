"
Does only whole pixel antialiasing. Appropriate for very high resolution displays, or slower systems where the extra computational load of VectorEngineSubPixel is not affordable.
Variables edgeCounts and alphaMask are single byte per pixel.
"
Class {
	#name : 'VectorEngineWholePixel',
	#superclass : 'VectorEngineSmalltalk',
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'instance creation' }
VectorEngineWholePixel class >> defaultAntiAliasingWidth [
	^1.6
]

{ #category : 'private' }
VectorEngineWholePixel >> auxPixelDataClass [
	"Because we hold data for whole pixels"

	^ByteArray
]

{ #category : 'private - blend with mask' }
VectorEngineWholePixel >> blendFillOnly [
	"Blends fill color over background.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]."

	| t b l r edgesUpToThisPixel edgesThisPixel pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realFillColor opaqueFillColorWord realOpaqueFillColorWord displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex |
	t := spanTop. b := spanBottom. l := spanLeft. r := spanRight.	"On the VectorEnginePlugin, they are different variables, of different type. Make code similar."
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1.
	mustResetColor := false.
	opaqueFillColorWord := 0.
	(targetAssumedOpaque and: [ fillColor alpha = 1.0 ]) ifTrue: [
		opaqueFillColorWord := (fillColor red * 255) rounded * 256+(fillColor green * 255) rounded * 256 + (fillColor blue*255) rounded + 16rFF000000 ].
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l + 1. "+1 solo en Smalltalk"
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			"pixelIndex >> 4 in Slang / C"
			affectedBitsIndex := pixelIndex-1>>4+1.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				alphasOrEdgeCountsInThisSegment ifTrue: [
					affectedBits at: affectedBitsIndex put: 0 ]].
			segmentLength := affectedBitsIndex << 4+1 - pixelIndex.
			alphasOrEdgeCountsInThisSegment  | (edgesUpToThisPixel ~= 0) ifTrue: [
				"Some alpha not zero in this segment of length delta, or painting inside the shape."
				(segmentLength min: (r-displayX+1)) timesRepeat: [
					
					strokeAntiAliasAlphaBits := 0.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCounts at: pixelIndex.
						edgesThisPixel = 0 ifFalse: [
							edgeCounts at: pixelIndex put: 0.
							"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
							edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ].
							edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
						strokeAntiAliasAlphaBits := alphaMask at: pixelIndex.
						strokeAntiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ]].
					
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
							ifTrue: [
								realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25.
								realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									realFillColor := fillColor.  fillColor := fillColor alpha: fillColor alpha * 0.75.
									realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]].
						
						edgesUpToThisPixel = 0
							ifFalse: [ "Inside the shape"
								(strokeAntiAliasAlphaBits = 0 and: [ opaqueFillColorWord ~= 0 ])
									ifTrue: [
										"Fully inside the shape, far from anti aliasing. Color is opaque. Target is too. Just overwrite with fill color."
										targetForm bits at: pixelIndex put: opaqueFillColorWord.
										morphIds at: pixelIndex put: currentMorphId ]
									ifFalse: [
										"Inside the shape. Turn stroke anti aliasing into fill anti aliasing."
										self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: 127 - strokeAntiAliasAlphaBits ]]
							ifTrue: [ "Still in the anti aliasing area, but outside the shape, strictly speaking."
								strokeAntiAliasAlphaBits = 0 ifFalse: [
									self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits ]].
						
						mustResetColor ifTrue: [
							fillColor := realFillColor. opaqueFillColorWord := realOpaqueFillColorWord. mustResetColor := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
			
		clippingSpecIndex := clippingSpecIndex + 2 ].
]

{ #category : 'private - blend with mask' }
VectorEngineWholePixel >> blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits [
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| resultR resultG resultB antiAliasAlpha alpha unAlpha resultAlpha targetWord targetAlpha |

	antiAliasAlpha := antiAliasAlphaBits / 127.0.
	alpha := antiAliasAlpha * fillColor alpha.
	unAlpha := 1.0 - alpha.

	targetWord := targetForm bits bytesAt: pixelIndex.
	targetAlpha := (targetWord at: 4) / 255.0.

	resultAlpha := alpha + (unAlpha * targetAlpha).
	targetWord at: 4 put: (resultAlpha*255) rounded.
	
	resultR := alpha * fillColor red * 255 + (unAlpha * (targetWord at: 3) * targetAlpha).
	resultR := resultR/resultAlpha.
	targetWord at: 3 put: resultR rounded.

	resultG := alpha * fillColor green * 255 + (unAlpha * (targetWord at: 2) * targetAlpha).
	resultG := resultG/resultAlpha.
	targetWord at: 2 put: resultG rounded.

	resultB := alpha * fillColor blue * 255 + (unAlpha * (targetWord at: 1) * targetAlpha).
	resultB := resultB/resultAlpha.
	targetWord at: 1 put: resultB rounded.

	targetForm bits bytesAt: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
]

{ #category : 'private - blend with mask' }
VectorEngineWholePixel >> blendStrokeAndFill [
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| t b l r edgesUpToThisPixel edgesThisPixel pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColors realStrokeColor realFillColor opaqueStrokeColorWord opaqueFillColorWord realOpaqueStrokeColorWord realOpaqueFillColorWord displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex |
	t := spanTop. b := spanBottom. l := spanLeft. r := spanRight.	"On the VectorEnginePlugin, they are different variables, of different type. Make code similar."
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1.
	mustResetColors := false.
	opaqueStrokeColorWord := 0.
	opaqueFillColorWord := 0.
	(targetAssumedOpaque and: [ strokeColor alpha * fillColor alpha = 1.0 ]) ifTrue: [
		opaqueStrokeColorWord := (strokeColor red * 255) rounded * 256+(strokeColor green * 255) rounded * 256 + (strokeColor blue*255) rounded + 16rFF000000.
		opaqueFillColorWord := (fillColor red * 255) rounded * 256+(fillColor green * 255) rounded * 256 + (fillColor blue*255) rounded + 16rFF000000 ].
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l + 1. "+1 solo en Smalltalk"
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			"pixelIndex >> 4 in Slang / C"
			affectedBitsIndex := pixelIndex-1>>4+1.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				alphasOrEdgeCountsInThisSegment ifTrue: [
					affectedBits at: affectedBitsIndex put: 0 ]].
			segmentLength := affectedBitsIndex << 4+1 - pixelIndex.
			alphasOrEdgeCountsInThisSegment  | (edgesUpToThisPixel ~= 0) ifTrue: [
				"Some alpha not zero in this segment of length delta, or painting inside the shape."
				(segmentLength min: (r-displayX+1)) timesRepeat: [
					
					strokeAntiAliasAlphaBits := 0.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCounts at: pixelIndex.
						edgesThisPixel = 0 ifFalse: [
							edgeCounts at: pixelIndex put: 0.
							"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
							edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ].
							edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
						strokeAntiAliasAlphaBits := alphaMask at: pixelIndex.
						strokeAntiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ]].
					
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel ])
							ifTrue: [
								realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25.
								realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25.
								realOpaqueStrokeColorWord := opaqueStrokeColorWord. opaqueStrokeColorWord := 0. 
								realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColors := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25.
									realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25.
									realOpaqueStrokeColorWord := opaqueStrokeColorWord. opaqueStrokeColorWord := 0. 
									realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColors := true ]].
						
						strokeAntiAliasAlphaBits = 0
							ifTrue: [ 		"Not in the stroke at all. Either fully in the fill, or outside the shape (pixel is unaffected)."
								edgesUpToThisPixel ~= 0
									ifTrue: [
										"Fully inside the fill, far from anti aliasing."
										opaqueFillColorWord ~= 0
											ifTrue: [
												"Fill color is opaque. Target is too. Just overwrite with fill color."
												targetForm bits at: pixelIndex put: opaqueFillColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of fill over target."
												self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: 127 ]]]
							ifFalse: [		"In the stroke."
								strokeAntiAliasAlphaBits = 127
									ifTrue: [
										"Fully inside the stroke, far from anti aliasing."
										opaqueStrokeColorWord ~= 0
											ifTrue: [
												"Stroke color is opaque. Target is too. Just overwrite with stroke color."
												targetForm bits at: pixelIndex put: opaqueStrokeColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of stroke over target."
												self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: 127 ]]
									ifFalse: [
										"In an anti aliased part of the stroke. Either blend stroke over background, or pre-mix stroke and fill."
										edgesUpToThisPixel ~= 0
											ifTrue: [ 
												"Inside the shape. Blend stroke and fill, blend result over target."
												self blendStrokeAndFillAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits ]
											ifFalse: [
												"In the outer anti aliasing area of the stroke. Blend stroke over background."
												self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits ]]].
						
						mustResetColors ifTrue: [
							strokeColor := realStrokeColor. fillColor := realFillColor.
							opaqueStrokeColorWord := realOpaqueStrokeColorWord. opaqueFillColorWord := realOpaqueFillColorWord.
							mustResetColors := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
			
		clippingSpecIndex := clippingSpecIndex + 2 ].
]

{ #category : 'private - blend with mask' }
VectorEngineWholePixel >> blendStrokeAndFillAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits [
	"Do an appropriate (anti aliased) mix between stoke color and fill color. Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB strokeAAAlpha alpha unAlpha foreR foreG foreB targetAlpha resultAlpha |

	strokeAAAlpha := strokeAntiAliasAlphaBits / 127.0.

	"Do gradient between stroke and fill. Blend the result over background"
	foreR := strokeAAAlpha * strokeColor red * 255 + ((1-strokeAAAlpha) * fillColor red * 255).
	foreG := strokeAAAlpha * strokeColor green * 255 + ((1-strokeAAAlpha) * fillColor green * 255).
	foreB := strokeAAAlpha * strokeColor blue * 255 + ((1-strokeAAAlpha) * fillColor blue * 255).
	alpha := strokeAAAlpha * strokeColor alpha + ((1-strokeAAAlpha) * fillColor alpha).
	unAlpha := 1.0 - alpha.

	targetWord := targetForm bits bytesAt: pixelIndex.
	targetAlpha := (targetWord at: 4) / 255.0.

	resultAlpha := alpha + (unAlpha * targetAlpha).
	targetWord at: 4 put: (resultAlpha*255) rounded.

	resultR := alpha * foreR + (unAlpha * (targetWord at: 3) * targetAlpha).
	resultR := resultR/resultAlpha.
	targetWord at: 3 put: resultR rounded.

	resultG := alpha * foreG + (unAlpha * (targetWord at: 2) * targetAlpha).
	resultG := resultG/resultAlpha.
	targetWord at: 2 put: resultG rounded.

	resultB := alpha * foreB + (unAlpha * (targetWord at: 1) * targetAlpha).
	resultB := resultB/resultAlpha.
	targetWord at: 1 put: resultB rounded.

	targetForm bits bytesAt: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
]

{ #category : 'private - blend with mask' }
VectorEngineWholePixel >> blendStrokeOnly [
	"Blends stroke color over background.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| t b l r pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realStrokeColor opaqueStrokeColorWord realOpaqueStrokeColorWord displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex |
	t := spanTop. b := spanBottom. l := spanLeft. r := spanRight.	"On the VectorEnginePlugin, they are different variables, of different type. Make code similar."
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1.
	mustResetColor := false.
	opaqueStrokeColorWord := 0.
	(targetAssumedOpaque and: [ strokeColor alpha = 1.0 ]) ifTrue: [
		opaqueStrokeColorWord := (strokeColor red * 255) rounded * 256+(strokeColor green * 255) rounded * 256 + (strokeColor blue*255) rounded + 16rFF000000 ].
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		pixelIndex := displayY * targetWidth + l + 1. "+1 solo en Smalltalk"
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			"pixelIndex >> 4 in Slang / C"
			affectedBitsIndex := pixelIndex-1>>4+1.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				alphasOrEdgeCountsInThisSegment ifTrue: [
					affectedBits at: affectedBitsIndex put: 0 ]].
			segmentLength := affectedBitsIndex << 4+1 - pixelIndex.
			alphasOrEdgeCountsInThisSegment ifTrue: [
				"Some alpha not zero in this segment of length delta"
				(segmentLength min: (r-displayX+1)) timesRepeat: [
					
					strokeAntiAliasAlphaBits := alphaMask at: pixelIndex.
					strokeAntiAliasAlphaBits = 0 ifFalse: [			"In the stroke"
						alphaMask at: pixelIndex put: 0.
						
						(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
							(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
								ifTrue: [
									realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25.
									realOpaqueStrokeColorWord := opaqueStrokeColorWord. realOpaqueStrokeColorWord := 0. mustResetColor := true ]
								ifFalse: [
									(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
										realStrokeColor := strokeColor.  strokeColor := strokeColor alpha: strokeColor alpha * 0.75.
										realOpaqueStrokeColorWord := opaqueStrokeColorWord. realOpaqueStrokeColorWord := 0. mustResetColor := true ]].
							
							(opaqueStrokeColorWord ~= 0 and: [strokeAntiAliasAlphaBits = 127])
								ifTrue: [
									"Optimize inner part of a wide stroke: Fully opaque stroke (and target), no anti aliasing, no clipping at this point."
									targetForm bits at: pixelIndex put: opaqueStrokeColorWord.
									morphIds at: pixelIndex put: currentMorphId ]
								ifFalse: [
									"In the anti aliasing area of the stroke, or color is translucent, or target translucency is desired."
									self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits ].
							
							mustResetColor ifTrue: [
								strokeColor := realStrokeColor. opaqueStrokeColorWord := realOpaqueStrokeColorWord. mustResetColor := false ]]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].
]

{ #category : 'private - blend with mask' }
VectorEngineWholePixel >> blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits [
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlpha alpha unAlpha resultAlpha targetAlpha |

	antiAliasAlpha := strokeAntiAliasAlphaBits / 127.0.
	alpha := antiAliasAlpha * strokeColor alpha.
	unAlpha := 1.0 - alpha.

	targetWord := targetForm bits bytesAt: pixelIndex.
	targetAlpha := (targetWord at: 4) / 255.0.

	resultAlpha := alpha + (unAlpha * targetAlpha).
	targetWord at: 4 put: (resultAlpha*255) rounded.

	resultR := alpha * strokeColor red * 255 + (unAlpha * (targetWord at: 3) * targetAlpha).
	resultR := resultR/resultAlpha.
	targetWord at: 3 put: resultR rounded.

	resultG := alpha * strokeColor green * 255 + (unAlpha * (targetWord at: 2) * targetAlpha).
	resultG := resultG/resultAlpha.
	targetWord at: 2 put: resultG rounded.

	resultB := alpha * strokeColor blue * 255 + (unAlpha * (targetWord at: 1) * targetAlpha).
	resultB := resultB/resultAlpha.
	targetWord at: 1 put: resultB rounded.

	targetForm bits bytesAt: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
]

{ #category : 'primitive algorithms - aux' }
VectorEngineWholePixel >> updateAlphasForX: x y: y [
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
	needsFullAlphaCircle := false.
]

{ #category : 'primitive algorithms - aux' }
VectorEngineWholePixel >> updateAlphasZeroStrokeForX: x y: y [
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
					alphaMask at: pixelIndex+1 put: alpha ]]]].
]

{ #category : 'primitive algorithms - aux' }
VectorEngineWholePixel >> updateEdgeCountAtX: x y: y [
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
	prevYTruncated := thisYTruncated.
]
