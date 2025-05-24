'From Cuis7.3 [latest update: #7123] on 9 April 2025 at 10:00:14 am'!

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 4/9/2025 09:20:07'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| t b l r edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realFillColor opaqueFillColorWord realOpaqueFillColorWord displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex |
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
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		isRedInside := isGreenInside := isBlueInside := false.
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
			alphasOrEdgeCountsInThisSegment | isGreenInside ifTrue: [
				"Some alpha not zero in this segment of length delta, or painting inside the shape."
				(segmentLength min: (r-displayX+1)) timesRepeat: [
					
					strokeAntiAliasAlphasWord := "16r00000000" #[16r00 16r00 16r00 16r00].
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCounts bytesAt: pixelIndex.
						edgesThisPixel ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
							edgeCounts at: pixelIndex put: 0.
							edgesThisPixelR := edgesThisPixel at: 3.
							edgesThisPixelG := edgesThisPixel at: 2.
							edgesThisPixelB := edgesThisPixel at: 1.
							"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
							edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR := edgesThisPixelR - 256 ].
							edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG := edgesThisPixelG - 256 ].
							edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB := edgesThisPixelB - 256 ].
							edgesUpToThisPixelR := edgesUpToThisPixelR + edgesThisPixelR.
							edgesUpToThisPixelG := edgesUpToThisPixelG + edgesThisPixelG.
							edgesUpToThisPixelB := edgesUpToThisPixelB + edgesThisPixelB.
							isRedInside := edgesUpToThisPixelR ~= 0.
							isGreenInside := edgesUpToThisPixelG ~= 0.
							isBlueInside := edgesUpToThisPixelB ~= 0 ].
						strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
						strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
							alphaMask at: pixelIndex put: 0 ]].
					
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
							ifTrue: [
								realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25.
								realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									realFillColor := fillColor.  fillColor := fillColor alpha: fillColor alpha * 0.75.
									realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]].
						
						(opaqueFillColorWord ~= 0 and: [strokeAntiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] and: [ isGreenInside ]])
							ifTrue: [ "If no alpha, and isGreenInside is true, isRedInside and isBlueInside are also true"
								"Fully inside the shape, far from anti aliasing. Color is opaque. Target is too. Just overwrite with fill color."
								targetForm bits at: pixelIndex put: opaqueFillColorWord.
								morphIds at: pixelIndex put: currentMorphId ]
							ifFalse: [
								"General case. (strokeAntiAliasAlphasWord = 0 and outside the shape means NOP)"
								(strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [ isGreenInside ]) ifTrue: [
									"If no alpha, and isGreenInside is true, isRedInside and isBlueInside are also true
									If there is any alpha, isRedInside, isGreenInside, isBlueInside may be different."
									self blendFillOnlyAt: pixelIndex
										redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
										antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].
						
						mustResetColor ifTrue: [
							fillColor := realFillColor. opaqueFillColorWord := realOpaqueFillColorWord. mustResetColor := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 4/9/2025 09:38:42'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| t b l r edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColors realStrokeColor realFillColor opaqueStrokeColorWord opaqueFillColorWord realOpaqueStrokeColorWord realOpaqueFillColorWord displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex |
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
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		isRedInside := isGreenInside := isBlueInside := false.
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
			alphasOrEdgeCountsInThisSegment | isGreenInside ifTrue: [
				"Some alpha not zero in this segment of length delta, or painting inside the shape."
				(segmentLength min: (r-displayX+1)) timesRepeat: [
					
					strokeAntiAliasAlphasWord := "16r00000000" #[16r00 16r00 16r00 16r00].
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCounts bytesAt: pixelIndex.
						edgesThisPixel ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
							edgeCounts at: pixelIndex put: 0.
							edgesThisPixelR := edgesThisPixel at: 3.
							edgesThisPixelG := edgesThisPixel at: 2.
							edgesThisPixelB := edgesThisPixel at: 1.
							"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
							edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR := edgesThisPixelR - 256 ].
							edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG := edgesThisPixelG - 256 ].
							edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB := edgesThisPixelB - 256 ].
							edgesUpToThisPixelR := edgesUpToThisPixelR + edgesThisPixelR.
							edgesUpToThisPixelG := edgesUpToThisPixelG + edgesThisPixelG.
							edgesUpToThisPixelB := edgesUpToThisPixelB + edgesThisPixelB.
							isRedInside := edgesUpToThisPixelR ~= 0.
							isGreenInside := edgesUpToThisPixelG ~= 0.
							isBlueInside := edgesUpToThisPixelB ~= 0 ].
						strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
						strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
							alphaMask at: pixelIndex put: 0 ]].
					
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
						
						strokeAntiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00]
							ifTrue: [ 		"Not in the stroke at all. Either fully in the fill, or outside the shape (pixel is unaffected)."
								isGreenInside
									ifTrue: [
										"Fully inside the fill, far from anti aliasing. (Here isGreenInside also implies isRedInside and isBlueInside)"
										opaqueFillColorWord ~= 0
											ifTrue: [
												"Fill color is opaque. Target is too. Just overwrite with fill color."
												targetForm bits at: pixelIndex put: opaqueFillColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of fill over target."
												self blendFillOnlyAt: pixelIndex
													redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
													antiAliasAlphasWord: strokeAntiAliasAlphasWord ]]]
							ifFalse: [		"At least one subpixel in the stroke."
								strokeAntiAliasAlphasWord = "16r007F7F7F" #[16r7F 16r7F 16r7F 16r00]
									ifTrue: [
										"Fully inside the stroke, far from anti aliasing."
										opaqueStrokeColorWord ~= 0
											ifTrue: [
												"Stroke color is opaque. Target is too. Just overwrite with stroke color."
												targetForm bits at: pixelIndex put: opaqueStrokeColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of stroke over target."
												self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: #[16r7F 16r7F 16r7F 16r00] ]]
									ifFalse: [
										"In an anti aliased part of the stroke. Either blend stroke over background, or pre-mix stroke and fill."
										self blendStrokeAndFillAt: pixelIndex
											redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
											antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].					
						
						mustResetColors ifTrue: [
							strokeColor := realStrokeColor. fillColor := realFillColor.
							opaqueStrokeColorWord := realOpaqueStrokeColorWord. opaqueFillColorWord := realOpaqueFillColorWord.
							mustResetColors := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

