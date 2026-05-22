"
Does sub pixel antialiasing, both for text and graphics. Appropriate for low to medium resolution displays, where subpixel antialiasing improves quality. Consumes more CPU than VectorEngineWholePixel.
Variables edgeCounts and alphaMask are 4 bytes per pixel (3 actually used).
"
Class {
	#name : 'VectorEngineSubPixel',
	#superclass : 'VectorEngineSmalltalk',
	#instVars : [
		'subPixelDelta'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'instance creation' }
VectorEngineSubPixel class >> defaultAntiAliasingWidth [
	^1.4
]

{ #category : 'instance creation' }
VectorEngineSubPixel class >> defaultSubPixelDelta [

	^0.25			"Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"
]

{ #category : 'private' }
VectorEngineSubPixel >> auxPixelDataClass [
	"Because we hold data for subpixels"

	^WordArray
]

{ #category : 'private - blend with mask' }
VectorEngineSubPixel >> blendFillOnly [
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
		
		clippingSpecIndex := clippingSpecIndex + 2 ].
]

{ #category : 'private - blend with mask' }
VectorEngineSubPixel >> blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord [
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha |

	"In this method, antiAliasAlphas are not used to blend stroke, but fill.
	This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0).
	Next lines try to be in a simd friendly style."
	alphaRBits := strokeAntiAliasAlphasWord at: 3.
	alphaGBits := strokeAntiAliasAlphasWord at: 2.
	alphaBBits := strokeAntiAliasAlphasWord at: 1.
	isRedInside ifTrue: [ alphaRBits := 127 - alphaRBits ].
	isGreenInside ifTrue: [ alphaGBits := 127 - alphaGBits ].
	isBlueInside ifTrue: [ alphaBBits := 127 - alphaBBits ].
	alphaR := alphaRBits / 127.0.
	alphaG := alphaGBits / 127.0.
	alphaB := alphaBBits / 127.0.
	alphaR := alphaR * fillColor alpha.
	alphaG := alphaG * fillColor alpha.
	alphaB := alphaB * fillColor alpha.

	targetWord := targetForm bits bytesAt: pixelIndex.
	targetAlpha := (targetWord at: 4) / 255.0.

	"These if are not really needed. just ignore them if we use simd instructions."
	alphaR = 0.0 ifFalse: [
		unAlphaR := 1.0 - alphaR.
		resultAlpha := alphaR + (unAlphaR * targetAlpha).
		resultR := alphaR * fillColor red * 255 + (unAlphaR * (targetWord at: 3) * targetAlpha).
		resultR := resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded ].

	alphaG = 0.0 ifFalse: [
		unAlphaG := 1.0 - alphaG.
		resultAlpha := alphaG + (unAlphaG * targetAlpha).
		resultG := alphaG * fillColor green * 255 + (unAlphaG * (targetWord at: 2) * targetAlpha).
		resultG := resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.
		targetWord at: 4 put: (resultAlpha*255) rounded ].

	alphaB = 0.0 ifFalse: [
		unAlphaB := 1.0 - alphaB.
		resultAlpha := alphaB + (unAlphaB * targetAlpha).
		resultB := alphaB * fillColor blue * 255 + (unAlphaB * (targetWord at: 1) * targetAlpha).
		resultB := resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded ].

	targetForm bits bytesAt: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
]

{ #category : 'private - blend with mask' }
VectorEngineSubPixel >> blendStrokeAndFill [
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
		
		clippingSpecIndex := clippingSpecIndex + 2 ].
]

{ #category : 'private - blend with mask' }
VectorEngineSubPixel >> blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord [
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB strokeAARedAlphaBits strokeAARedAlpha strokeAAGreenAlphaBits strokeAAGreenAlpha strokeAABlueAlphaBits strokeAABlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB foreR foreG foreB targetAlpha resultAlpha |

	"Next lines try to be in a simd friendly style."
	strokeAARedAlphaBits := strokeAntiAliasAlphasWord at: 3.
	strokeAAGreenAlphaBits := strokeAntiAliasAlphasWord at: 2.
	strokeAABlueAlphaBits := strokeAntiAliasAlphasWord at: 1.
	strokeAARedAlpha := strokeAARedAlphaBits / 127.0.
	strokeAAGreenAlpha := strokeAAGreenAlphaBits / 127.0.
	strokeAABlueAlpha := strokeAABlueAlphaBits / 127.0.

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR := strokeAARedAlpha * strokeColor alpha + ((1.0-strokeAARedAlpha) * fillColor alpha).
			foreR := strokeAARedAlpha * strokeColor red * 255 + ((1.0-strokeAARedAlpha) * fillColor red * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaR := strokeAARedAlpha * strokeColor alpha.
			foreR := strokeColor red * 255 ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG := strokeAAGreenAlpha * strokeColor alpha + ((1.0-strokeAAGreenAlpha) * fillColor alpha).
			foreG := strokeAAGreenAlpha * strokeColor green * 255 + ((1.0-strokeAAGreenAlpha) * fillColor green * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaG := strokeAAGreenAlpha * strokeColor alpha.
			foreG := strokeColor green * 255 ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB := strokeAABlueAlpha * strokeColor alpha + ((1.0-strokeAABlueAlpha) * fillColor alpha).
			foreB := strokeAABlueAlpha * strokeColor blue * 255 + ((1.0-strokeAABlueAlpha) * fillColor blue * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaB := strokeAABlueAlpha * strokeColor alpha.
			foreB := strokeColor blue * 255 ].

	targetWord := targetForm bits bytesAt: pixelIndex.
	targetAlpha := (targetWord at: 4) / 255.0.

	"These if are not really needed. just ignore them if we use simd instructions."
	alphaR = 0.0 ifFalse: [
		unAlphaR := 1.0 - alphaR.
		resultAlpha := alphaR + (unAlphaR * targetAlpha).
		resultR := alphaR * foreR + (unAlphaR * (targetWord at: 3) * targetAlpha).
		resultR := resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded ].
	alphaG = 0.0 ifFalse: [
		unAlphaG := 1.0 - alphaG.
		resultAlpha := alphaG + (unAlphaG * targetAlpha).
		resultG := alphaG * foreG + (unAlphaG * (targetWord at: 2) * targetAlpha).
		resultG := resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.
		targetWord at: 4 put: (resultAlpha*255) rounded ].
	alphaB = 0.0 ifFalse: [
		unAlphaB := 1.0 - alphaB.
		resultAlpha := alphaB + (unAlphaB * targetAlpha).
		resultB := alphaB * foreB + (unAlphaB * (targetWord at: 1) * targetAlpha).
		resultB := resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded ].

	targetForm bits bytesAt: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
]

{ #category : 'private - blend with mask' }
VectorEngineSubPixel >> blendStrokeOnly [
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| t b l r pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realStrokeColor opaqueStrokeColorWord realOpaqueStrokeColorWord displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex |
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
					
					strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
					"At least one subpixel in the stroke."
					strokeAntiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifFalse: [
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
							
							(opaqueStrokeColorWord ~= 0 and: [strokeAntiAliasAlphasWord = "16r007F7F7F" #[16r7F 16r7F 16r7F 16r00]])
								ifTrue: [
									"Fully inside the stroke, far from anti aliasing. Color is opaque. Target is too. Just overwrite with stroke color."
									targetForm bits at: pixelIndex put: opaqueStrokeColorWord.
									morphIds at: pixelIndex put: currentMorphId ]
								ifFalse: [
									"At least one subpixel in the anti aliasing area of the stroke, or color is translucent, or target translucency is desired."
									self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord ].
							
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
VectorEngineSubPixel >> blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord [
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha |

	"Next lines try to be in a simd friendly style."
	alphaRBits := strokeAntiAliasAlphasWord at: 3.
	alphaGBits := strokeAntiAliasAlphasWord at: 2.
	alphaBBits := strokeAntiAliasAlphasWord at: 1.
	alphaR := alphaRBits / 127.0.
	alphaG := alphaGBits / 127.0.
	alphaB := alphaBBits / 127.0.
	alphaR := alphaR * strokeColor alpha.
	alphaG := alphaG * strokeColor alpha.
	alphaB := alphaB * strokeColor alpha.

	targetWord := targetForm bits bytesAt: pixelIndex.
	targetAlpha := (targetWord at: 4) / 255.0.

	"These if are not really needed. just ignore them if we use simd instructions."
	alphaR = 0.0 ifFalse: [
		unAlphaR := 1.0 - alphaR.
		resultAlpha := alphaR + (unAlphaR * targetAlpha).
		resultR := alphaR * strokeColor red * 255 + (unAlphaR * (targetWord at: 3) * targetAlpha).
		resultR := resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded ].

	alphaG = 0.0 ifFalse: [
		unAlphaG := 1.0 - alphaG.
		resultAlpha := alphaG + (unAlphaG * targetAlpha).
		resultG := alphaG * strokeColor green * 255 + (unAlphaG * (targetWord at: 2) * targetAlpha).
		resultG := resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.
		targetWord at: 4 put: (resultAlpha*255) rounded ].

	alphaB = 0.0 ifFalse: [
		unAlphaB := 1.0 - alphaB.
		resultAlpha := alphaB + (unAlphaB * targetAlpha).
		resultB := alphaB * strokeColor blue * 255 + (unAlphaB * (targetWord at: 1) * targetAlpha).
		resultB := resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded ].

	targetForm bits bytesAt: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
]

{ #category : 'accessing' }
VectorEngineSubPixel >> disableSubPixelSampling [
	subPixelDelta := 0.0
]

{ #category : 'private' }
VectorEngineSubPixel >> pvtSetForm: aForm [

	super pvtSetForm: aForm.
	subPixelDelta := self class defaultSubPixelDelta.
]

{ #category : 'accessing' }
VectorEngineSubPixel >> subPixelDelta [
	^subPixelDelta
]

{ #category : 'primitive algorithms - aux' }
VectorEngineSubPixel >> updateAlphasForX: x y: y [
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
	needsFullAlphaCircle := false.
]

{ #category : 'primitive algorithms - aux' }
VectorEngineSubPixel >> updateAlphasZeroStrokeForX: x y: y [
	"This optimization is currently only implemented for the Whole Pixel engine.
	See implementors and senders."

	self updateAlphasForX: x y: y
]

{ #category : 'primitive algorithms - aux' }
VectorEngineSubPixel >> updateEdgeCountAtX: x y: y [
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
		].
]
