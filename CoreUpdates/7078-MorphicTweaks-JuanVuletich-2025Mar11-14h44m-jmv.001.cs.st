'From Cuis7.3 [latest update: #7069] on 11 March 2025 at 2:54:27 pm'!

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/11/2025 11:41:34'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex antiAliasAlphaBits |
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
			antiAliasAlphaBits := alphaMask at: pixelIndex.
			antiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ].
			edgesUpToThisPixel = 0
				ifFalse: [		"Inside the shape. Turn stroke anti aliasing into fill anti aliasing."
					antiAliasAlphaBits := 127 - antiAliasAlphaBits.
					self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]
				ifTrue: [		"Still in the anti aliasing area, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 0 ifFalse: [
						self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/11/2025 10:26:18'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex antiAliasAlphaBits |
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
			antiAliasAlphaBits := alphaMask at: pixelIndex.
			antiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ].
			edgesUpToThisPixel = 0
				ifFalse: [			"Inside the shape"
					self blendStrokeAndFillAt: pixelIndex isInside: true antiAliasAlphaByte: antiAliasAlphaBits ]
				ifTrue: [			"Still in the stroke, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 0 ifFalse: [
						self blendStrokeAndFillAt: pixelIndex isInside: false antiAliasAlphaByte: antiAliasAlphaBits ]]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/11/2025 14:51:14'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex antiAliasAlphaBits |
	spanTop to: spanBottom do: [ :displayY |
		pixelIndex := displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			antiAliasAlphaBits := alphaMask at: pixelIndex.
			antiAliasAlphaBits = 0 ifFalse: [						"In the stroke"
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]].! !

