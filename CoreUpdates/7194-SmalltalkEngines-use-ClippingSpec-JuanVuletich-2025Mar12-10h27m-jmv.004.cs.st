'From Cuis7.3 [latest update: #7077] on 12 March 2025 at 12:24:19 pm'!

!Sample01Star methodsFor: 'drawing' stamp: 'jmv 3/12/2025 12:22:22'!
drawOn: aCanvas
	"A 5 point star.
	Feel free to experiment with this code.
	The worst thing that could happen is that your morph breaks. That will only mean extra fun!!
	Sample01Star new openInHand
	"

	aCanvas strokeWidth: 12 color: Color lightOrange do: [
	"aCanvas fillColor: Color lightOrange do: ["
	"aCanvas strokeWidth: 12 color: Color lightOrange fillColor: Color yellow do: ["
		aCanvas
				moveTo: (Point rho: 100 theta: 90 degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*2+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*4+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*6+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: (360/5*8+90) degreesToRadians);
				lineTo: (Point rho: 100 theta: 90 degreesToRadians).
		].! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 3/12/2025 12:19:03'!
onForm: aForm translation: aPoint

	^ self subclassToUse basicNew
		setForm: aForm subPixelAntiAliasing: false" not";
		initializeWithTranslation: aPoint.! !


!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:17:45'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedLeftPixel antiAliasedRightPixel mustResetColor realFillColor |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedLeftPixel := targetWidth."targetWidth is effectively NOP"
	antiAliasedRightPixel := targetWidth."targetWidth is effectively NOP"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
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
				(displayX = antiAliasedLeftPixel or: [ displayX = antiAliasedRightPixel])
					ifTrue: [ realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25. mustResetColor := true ]
					ifFalse: [
						(displayX-1 = antiAliasedLeftPixel or: [ displayX+1 = antiAliasedRightPixel]) ifTrue: [
							realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.75. mustResetColor := true ]].
				(strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
					isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
						"At least one subpixel inside the shape, or in the anti aliasing area"
						self blendFillOnlyAt: pixelIndex
							redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
							antiAliasAlphasWord: strokeAntiAliasAlphasWord ].
				mustResetColor ifTrue: [
					fillColor := realFillColor. mustResetColor := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:17:50'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedLeftPixel antiAliasedRightPixel mustResetColors realStrokeColor realFillColor |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedLeftPixel := targetWidth."targetWidth is effectively NOP"
	antiAliasedRightPixel := targetWidth."targetWidth is effectively NOP"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColors := false.
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
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
				(displayX = antiAliasedLeftPixel or: [ displayX = antiAliasedRightPixel])
					ifTrue: [
						realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25.
						realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25. mustResetColors := true ]
					ifFalse: [
						(displayX-1 = antiAliasedLeftPixel or: [ displayX+1 = antiAliasedRightPixel]) ifTrue: [
							realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.75.
							realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.75. mustResetColors := true ]].
				(strokeAntiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
					isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
						"At least one subpixel in the stroke or inside the shape"
						self blendStrokeAndFillAt: pixelIndex
							redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
							antiAliasAlphasWord: strokeAntiAliasAlphasWord ].mustResetColors ifTrue: [
				strokeColor := realStrokeColor. fillColor := realFillColor. mustResetColors := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:17:53'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedLeftPixel antiAliasedRightPixel mustResetColor realStrokeColor |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedLeftPixel := targetWidth."targetWidth is effectively NOP"
	antiAliasedRightPixel := targetWidth."targetWidth is effectively NOP"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			strokeAntiAliasAlphasWord := alphaMask bytesAt: pixelIndex.
			"At least one subpixel in the stroke."
			strokeAntiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifFalse: [
				alphaMask at: pixelIndex put: 0.
				(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
					(displayX = antiAliasedLeftPixel or: [ displayX = antiAliasedRightPixel])
						ifTrue: [
							realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25. mustResetColor := true ]
						ifFalse: [
							(displayX-1 = antiAliasedLeftPixel or: [ displayX+1 = antiAliasedRightPixel]) ifTrue: [
								realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.75. mustResetColor := true ]].
					self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord.
					mustResetColor ifTrue: [
						strokeColor := realStrokeColor. mustResetColor := false ]]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !


!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:07:06'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex antiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedLeftPixel antiAliasedRightPixel mustResetColor realFillColor |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedLeftPixel := targetWidth."targetWidth is effectively NOP"
	antiAliasedRightPixel := targetWidth."targetWidth is effectively NOP"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
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
				(displayX = antiAliasedLeftPixel or: [ displayX = antiAliasedRightPixel])
					ifTrue: [ realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25. mustResetColor := true ]
					ifFalse: [
						(displayX-1 = antiAliasedLeftPixel or: [ displayX+1 = antiAliasedRightPixel]) ifTrue: [
							realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.75. mustResetColor := true ]].
				edgesUpToThisPixel = 0
					ifFalse: [		"Inside the shape. Turn stroke anti aliasing into fill anti aliasing."
						antiAliasAlphaBits := 127 - antiAliasAlphaBits.
						self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]
					ifTrue: [		"Still in the anti aliasing area, but outside the shape, strictly speaking."
						antiAliasAlphaBits = 0 ifFalse: [
							self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]].
				mustResetColor ifTrue: [
					fillColor := realFillColor. mustResetColor := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:07:23'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedLeftPixel antiAliasedRightPixel mustResetColors realStrokeColor realFillColor |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedLeftPixel := targetWidth."targetWidth is effectively NOP"
	antiAliasedRightPixel := targetWidth."targetWidth is effectively NOP"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColors := false.
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
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
				(displayX = antiAliasedLeftPixel or: [ displayX = antiAliasedRightPixel])
					ifTrue: [
						realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25.
						realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.25. mustResetColors := true ]
					ifFalse: [
						(displayX-1 = antiAliasedLeftPixel or: [ displayX+1 = antiAliasedRightPixel]) ifTrue: [
							realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.75.
							realFillColor := fillColor. fillColor := fillColor alpha: fillColor alpha * 0.75. mustResetColors := true ]].
				edgesUpToThisPixel = 0
					ifFalse: [			"Inside the shape"
						self blendStrokeAndFillAt: pixelIndex isInside: true antiAliasAlphaByte: strokeAntiAliasAlphaBits ]
					ifTrue: [			"Still in the stroke, but outside the shape, strictly speaking."
						strokeAntiAliasAlphaBits = 0 ifFalse: [
							self blendStrokeAndFillAt: pixelIndex isInside: false antiAliasAlphaByte: strokeAntiAliasAlphaBits ]].
				mustResetColors ifTrue: [
					strokeColor := realStrokeColor. fillColor := realFillColor. mustResetColors := false ]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:07:32'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedLeftPixel antiAliasedRightPixel mustResetColor realStrokeColor |
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedLeftPixel := targetWidth."targetWidth is effectively NOP"
	antiAliasedRightPixel := targetWidth."targetWidth is effectively NOP"
	clippingSpecIndex := spanTop * 2 + 1.
	mustResetColor := false.
	spanTop to: spanBottom do: [ :displayY |
		clippingSpec ifNotNil: [ "Not needed if moved to a clippingSpec specific method."
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedLeftPixel := clippingSpecL >= spanLeft ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedRightPixel := clippingSpecR <= spanRight ifTrue: [clippingSpecR] ifFalse:  [targetWidth]].
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			strokeAntiAliasAlphaBits := alphaMask at: pixelIndex.
			strokeAntiAliasAlphaBits = 0 ifFalse: [			"In the stroke"
				alphaMask at: pixelIndex put: 0.
				(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
					(displayX = antiAliasedLeftPixel or: [ displayX = antiAliasedRightPixel])
						ifTrue: [
							realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.25. mustResetColor := true ]
						ifFalse: [
							(displayX-1 = antiAliasedLeftPixel or: [ displayX+1 = antiAliasedRightPixel]) ifTrue: [
								realStrokeColor := strokeColor. strokeColor := strokeColor alpha: strokeColor alpha * 0.75. mustResetColor := true ]].
					self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits.
					mustResetColor ifTrue: [
						strokeColor := realStrokeColor. mustResetColor := false ]]]].
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

