'From Cuis7.3 [latest update: #7081] on 18 March 2025 at 4:48:23 pm'!

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex antiAliasAlphasWord |
	spanTop to: spanBottom do: [ :displayY |
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

			antiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
			antiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
					"At least one subpixel inside the shape, or in the anti aliasing area"
					self blendFillOnlyAt: pixelIndex
						redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
						antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:39:19'!
blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: antiAliasAlphasWord
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha clippingAntiAlias |

	"In this method, antiAliasAlphas are not used to blend stroke, but fill.
	This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0).
	Next lines try to be in a simd friendly style."
	alphaRBits := antiAliasAlphasWord at: 3.
	alphaGBits := antiAliasAlphasWord at: 2.
	alphaBBits := antiAliasAlphasWord at: 1.
	isRedInside ifTrue: [ alphaRBits := 127 - alphaRBits ].
	isGreenInside ifTrue: [ alphaGBits := 127 - alphaGBits ].
	isBlueInside ifTrue: [ alphaBBits := 127 - alphaBBits ].
	alphaR := alphaRBits / 127.0.
	alphaG := alphaGBits / 127.0.
	alphaB := alphaBBits / 127.0.
	alphaR := alphaR * fillColor alpha.
	alphaG := alphaG * fillColor alpha.
	alphaB := alphaB * fillColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias := clippingAntiAlias max: alphaGBits]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					alphaR := alphaR * clippingAntiAlias / 127.0.
					alphaG := alphaG * clippingAntiAlias / 127.0.
					alphaB := alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias := 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
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
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 09:00:37'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord |
	spanTop to: spanBottom do: [ :displayY |
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
			(strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
					"At least one subpixel in the stroke or inside the shape"
					self blendStrokeAndFillAt: pixelIndex
						redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
						antiAliasAlphasWord: strokeAntiAliasAlphasWord ]]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:42:49'!
blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB strokeAARedAlphaBits strokeAARedAlpha strokeAAGreenAlphaBits strokeAAGreenAlpha strokeAABlueAlphaBits strokeAABlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB foreR foreG foreB targetAlpha resultAlpha clippingAntiAlias |

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

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isGreenInside
				ifTrue: [clippingAntiAlias := 127]
				ifFalse: [
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					clippingAntiAlias := clippingAntiAlias max: strokeAAGreenAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					alphaR := alphaR * clippingAntiAlias / 127.0.
					alphaG := alphaG * clippingAntiAlias / 127.0.
					alphaB := alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias := 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
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
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 09:00:49'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex strokeAntiAliasAlphasWord |
	spanTop to: spanBottom do: [ :displayY |
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
			"At least one subpixel in the stroke."
			strokeAntiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifFalse: [
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord ]]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:44:47'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha clippingAntiAlias |

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

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias := clippingAntiAlias max: alphaGBits]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					alphaR := alphaR * clippingAntiAlias / 127.0.
					alphaG := alphaG * clippingAntiAlias / 127.0.
					alphaB := alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias := 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
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
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !


!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 09:01:25'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex strokeAntiAliasAlphaBits |
	spanTop to: spanBottom do: [ :displayY |
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
			edgesUpToThisPixel = 0
				ifFalse: [			"Inside the shape"
					self blendStrokeAndFillAt: pixelIndex isInside: true antiAliasAlphaByte: strokeAntiAliasAlphaBits ]
				ifTrue: [			"Still in the stroke, but outside the shape, strictly speaking."
					strokeAntiAliasAlphaBits = 0 ifFalse: [
						self blendStrokeAndFillAt: pixelIndex isInside: false antiAliasAlphaByte: strokeAntiAliasAlphaBits ]]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:47:11'!
blendStrokeAndFillAt: pixelIndex isInside: isInside antiAliasAlphaByte: strokeAntiAliasAlphaBits
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB strokeAAAlpha alpha unAlpha foreR foreG foreB targetAlpha resultAlpha clippingAntiAlias |

	strokeAAAlpha := strokeAntiAliasAlphaBits / 127.0.

	isInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alpha := strokeAAAlpha * strokeColor alpha + ((1-strokeAAAlpha) * fillColor alpha).
			foreR := strokeAAAlpha * strokeColor red * 255 + ((1-strokeAAAlpha) * fillColor red * 255).
			foreG := strokeAAAlpha * strokeColor green * 255 + ((1-strokeAAAlpha) * fillColor green * 255).
			foreB := strokeAAAlpha * strokeColor blue * 255 + ((1-strokeAAAlpha) * fillColor blue * 255) ]
		ifFalse: [							"Blend stroke over background"
			alpha := strokeAAAlpha * strokeColor alpha.
			foreR := strokeColor red * 255.
			foreG := strokeColor green * 255.
			foreB := strokeColor blue * 255 ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isInside
				ifTrue: [clippingAntiAlias := 127]
				ifFalse: [
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					clippingAntiAlias := clippingAntiAlias max: strokeAntiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					alpha := alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias := 0]].

	alpha = 0.0 ifFalse: [
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
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 09:02:00'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex strokeAntiAliasAlphaBits |
	spanTop to: spanBottom do: [ :displayY |
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			strokeAntiAliasAlphaBits := alphaMask at: pixelIndex.
			strokeAntiAliasAlphaBits = 0 ifFalse: [			"In the stroke"
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits ]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:47:42'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alpha unAlpha resultAlpha targetAlpha clippingAntiAlias |
	alpha := strokeAntiAliasAlphaBits / 127.0.
	alpha := alpha * strokeColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias := clippingAntiAlias max: strokeAntiAliasAlphaBits ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias := (morphIds at: pixelIndex) bitAnd: 127.
					alpha := alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias := 0]].

	alpha = 0.0 ifFalse: [
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
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

