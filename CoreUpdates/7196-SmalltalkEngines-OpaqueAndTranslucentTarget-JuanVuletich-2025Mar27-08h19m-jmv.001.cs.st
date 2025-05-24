'From Cuis7.3 [latest update: #7092] on 27 March 2025 at 8:30:23 am'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec targetAssumedOpaque '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/21/2025 14:53:58'!
disableTargetTranslucency
	"Avoid the cost of computing correct target transucency.
	This is the default behavior. Target translucency is not required when drawing to Display for live Morphic.
	See #enableTargetTranslucency."

	targetAssumedOpaque := true! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/21/2025 14:52:30'!
enableTargetTranslucency
	"Compute correct target transucency.
	This is usually not needed, and it is computationally more expensive. Default is #disableTargetTranslucency.
	It is required, for instance, for #imageForm and similar methods, so result is transparent outside of the drawing."

	targetAssumedOpaque := false! !


!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:30:41'!
blendStrokeAndFillAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
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
	morphIds at: pixelIndex put: currentMorphId.! !


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 3/20/2025 09:13:45'!
pvtSetForm: aForm

	| s |
	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	targetAssumedOpaque := true. 		"Set to false for true translucency on target."
	s := targetWidth * targetHeight.
	morphIds := WordArray new: s.
	edgeCounts := self auxPixelDataClass new: s.
	alphaMask := self auxPixelDataClass new: s.
	contour := Float32Array new: targetHeight * 2.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !


!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 16:49:36'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realFillColor opaqueFillColorWord realOpaqueFillColorWord |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	opaqueFillColorWord := 0.
	(targetAssumedOpaque and: [ fillColor alpha = 1.0 ]) ifTrue: [
		opaqueFillColorWord := (fillColor red * 255) rounded * 256+(fillColor green * 255) rounded * 256 + (fillColor blue*255) rounded + 16rFF000000 ].
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			
			edgesThisPixel := edgeCounts bytesAt: pixelIndex.
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
			isBlueInside := edgesUpToThisPixelB ~= 0.
			
			strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
			strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
				(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
					ifTrue: [
						realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25.
						realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]
					ifFalse: [
						(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
							realFillColor := fillColor.  fillColor := fillColor alpha: fillColor alpha * 0.75.
							realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]].
				
				(strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
					isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
					
						(strokeAntiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] and: [ opaqueFillColorWord ~= 0 ])
							ifTrue: [
								"Fully inside the shape, far from anti aliasing. Color is opaque. Target is too. Just overwrite with fill color."
								targetForm bits at: pixelIndex put: opaqueFillColorWord.
								morphIds at: pixelIndex put: currentMorphId ]
							ifFalse: [
								"At least one subpixel inside the shape, or in the anti aliasing area"
								self blendFillOnlyAt: pixelIndex
									redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
									antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].
				
				mustResetColor ifTrue: [
					fillColor := realFillColor. opaqueFillColorWord := realOpaqueFillColorWord. mustResetColor := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:24:32'!
blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord
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
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:15:57'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColors realStrokeColor realFillColor opaqueStrokeColorWord opaqueFillColorWord realOpaqueStrokeColorWord realOpaqueFillColorWord |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColors := false.
	opaqueStrokeColorWord := 0.
	opaqueFillColorWord := 0.
	(targetAssumedOpaque and: [ strokeColor alpha * fillColor alpha = 1.0 ]) ifTrue: [
		opaqueStrokeColorWord := (strokeColor red * 255) rounded * 256+(strokeColor green * 255) rounded * 256 + (strokeColor blue*255) rounded + 16rFF000000.
		opaqueFillColorWord := (fillColor red * 255) rounded * 256+(fillColor green * 255) rounded * 256 + (fillColor blue*255) rounded + 16rFF000000 ].
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			
			edgesThisPixel := edgeCounts bytesAt: pixelIndex.
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
			isBlueInside := edgesUpToThisPixelB ~= 0.
			
			strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
			strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
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
										self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord ]]
							ifFalse: [
								"In an anti aliased part of the stroke. Either blend stroke over background, or pre-mix stroke and fill."
								self blendStrokeAndFillAt: pixelIndex
									redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
									antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].					
				
				mustResetColors ifTrue: [
					strokeColor := realStrokeColor. fillColor := realFillColor.
					opaqueStrokeColorWord := realOpaqueStrokeColorWord. opaqueFillColorWord := realOpaqueFillColorWord.
					mustResetColors := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:25:21'!
blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord
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
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:06:56'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realStrokeColor opaqueStrokeColorWord realOpaqueStrokeColorWord |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	opaqueStrokeColorWord := 0.
	(targetAssumedOpaque and: [ strokeColor alpha = 1.0 ]) ifTrue: [
		opaqueStrokeColorWord := (strokeColor red * 255) rounded * 256+(strokeColor green * 255) rounded * 256 + (strokeColor blue*255) rounded + 16rFF000000 ].
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
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
						strokeColor := realStrokeColor. opaqueStrokeColorWord := realOpaqueStrokeColorWord. mustResetColor := false ]]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:25:58'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord
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
	morphIds at: pixelIndex put: currentMorphId.! !


!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:00:48'!
blendFillOnly
	"Blends fill color over background.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]."

	| edgesUpToThisPixel edgesThisPixel pixelIndex antiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realFillColor opaqueFillColorWord realOpaqueFillColorWord |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	opaqueFillColorWord := 0.
	(targetAssumedOpaque and: [ fillColor alpha = 1.0 ]) ifTrue: [
		opaqueFillColorWord := (fillColor red * 255) rounded * 256+(fillColor green * 255) rounded * 256 + (fillColor blue*255) rounded + 16rFF000000 ].
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			edgesThisPixel := edgeCounts at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0.
				"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
				edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ].
				edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits := alphaMask at: pixelIndex.
			antiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ].
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
						(antiAliasAlphaBits = 0 and: [ opaqueFillColorWord ~= 0 ])
							ifTrue: [
								"Fully inside the shape, far from anti aliasing. Color is opaque. Target is too. Just overwrite with fill color."
								targetForm bits at: pixelIndex put: opaqueFillColorWord.
								morphIds at: pixelIndex put: currentMorphId ]
							ifFalse: [
								"Inside the shape. Turn stroke anti aliasing into fill anti aliasing."
								self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: 127 - antiAliasAlphaBits ]]
					ifTrue: [ "Still in the anti aliasing area, but outside the shape, strictly speaking."
						antiAliasAlphaBits = 0 ifFalse: [
							self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]].
				
				mustResetColor ifTrue: [
					fillColor := realFillColor. opaqueFillColorWord := realOpaqueFillColorWord. mustResetColor := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:31:00'!
blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
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
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:12:47'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColors realStrokeColor realFillColor opaqueStrokeColorWord opaqueFillColorWord realOpaqueStrokeColorWord realOpaqueFillColorWord |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColors := false.
	opaqueStrokeColorWord := 0.
	opaqueFillColorWord := 0.
	(targetAssumedOpaque and: [ strokeColor alpha * fillColor alpha = 1.0 ]) ifTrue: [
		opaqueStrokeColorWord := (strokeColor red * 255) rounded * 256+(strokeColor green * 255) rounded * 256 + (strokeColor blue*255) rounded + 16rFF000000.
		opaqueFillColorWord := (fillColor red * 255) rounded * 256+(fillColor green * 255) rounded * 256 + (fillColor blue*255) rounded + 16rFF000000 ].
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			edgesThisPixel := edgeCounts at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0.
				"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
				edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ].
				edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
			strokeAntiAliasAlphaBits := alphaMask at: pixelIndex.
			strokeAntiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ].
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
										"Color is opaque. Target is too. Just overwrite with fill color."
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
										"Color is opaque. Target is too. Just overwrite with stroke color."
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
					mustResetColors := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:09:12'!
blendStrokeOnly
	"Blends stroke color over background.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realStrokeColor opaqueStrokeColorWord realOpaqueStrokeColorWord |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	opaqueStrokeColorWord := 0.
	(targetAssumedOpaque and: [ strokeColor alpha = 1.0 ]) ifTrue: [
		opaqueStrokeColorWord := (strokeColor red * 255) rounded * 256+(strokeColor green * 255) rounded * 256 + (strokeColor blue*255) rounded + 16rFF000000 ].
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
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
							"Fully inside the stroke, far from anti aliasing. Color is opaque. Target is too. Just overwrite with stroke color."
							targetForm bits at: pixelIndex put: opaqueStrokeColorWord.
							morphIds at: pixelIndex put: currentMorphId ]
						ifFalse: [
							"At least one subpixel in the anti aliasing area of the stroke, or color is translucent, or target translucency is desired."
							self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits ].
					
					mustResetColor ifTrue: [
						strokeColor := realStrokeColor. opaqueStrokeColorWord := realOpaqueStrokeColorWord. mustResetColor := false ]]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/26/2025 17:31:34'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
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
	morphIds at: pixelIndex put: currentMorphId.! !

!methodRemoval: VectorEngineWholePixel #blendStrokeAndFillAt:isInside:antiAliasAlphaByte: stamp: 'jmv 3/27/2025 08:20:32'!
VectorEngineWholePixel removeSelector: #blendStrokeAndFillAt:isInside:antiAliasAlphaByte:!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight targetAssumedOpaque morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId clippingSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!VectorEngineSubPixel reorganize!
('accessing' disableSubPixelSampling subPixelDelta)
('primitive algorithms - aux' updateAlphasForX:y: updateEdgeCountAtX:y:)
('private' auxPixelDataClass pvtSetForm:)
('private - blend with mask' blendFillOnly blendFillOnlyAt:redIsInside:greenIsInside:blueIsInside:antiAliasAlphasWord: blendStrokeAndFill blendStrokeAndFillAt:redIsInside:greenIsInside:blueIsInside:antiAliasAlphasWord: blendStrokeOnly blendStrokeOnlyAt:antiAliasAlphasWord:)
!


!VectorEngineWholePixel reorganize!
('primitive algorithms - aux' updateAlphasForX:y: updateEdgeCountAtX:y:)
('private' auxPixelDataClass)
('private - blend with mask' blendFillOnly blendFillOnlyAt:antiAliasAlphaByte: blendStrokeAndFill blendStrokeAndFillAt:antiAliasAlphaByte: blendStrokeOnly blendStrokeOnlyAt:antiAliasAlphaByte:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

VectorEngineDrawer allSubInstancesDo: [ :engine |
	engine instVarNamed: 'targetAssumedOpaque' put:​ true ].!

