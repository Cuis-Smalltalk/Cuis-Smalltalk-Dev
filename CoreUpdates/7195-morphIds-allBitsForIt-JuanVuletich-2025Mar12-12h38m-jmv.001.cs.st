'From Cuis7.3 [latest update: #7078] on 12 March 2025 at 12:42:56 pm'!

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/12/2025 12:38:20'!
image: srcForm at: aPoint
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need two of them!!"
	(world notNil and: [currentMorph == world]) ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: 0; sourceY: 0;
			destX: p x destY: p y width: srcForm width height: srcForm height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (srcForm boundingBox translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: 0; sourceY: 0;
				destX: p x destY: p y width: srcForm width height: srcForm height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			srcBox := srcForm boundingBox.
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (srcBox translatedBy: aPoint).
			dstBox := dstBox origin extent: dstBox extent + 1.1.
			dstBox := ((currentTransformation externalBoundingRectOf: dstBox))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !

!VectorCanvas methodsFor: 'drawing - MorphicCanvas protocol' stamp: 'jmv 3/12/2025 12:38:29'!
image: srcForm at: aPoint sourceRect: sourceRect
	"Workaround using WarpBlt until properly implemented in VectorEngine.
	 (WarpBlt might not give desired result, i.e. a properly filtered and resamples image)

	In the general case, use WarpBlt to apply currentTransformation.
	Prior to that, set morphIds for affected pixels, and update bounds in engine.
	This is expensive!!

	If No scaling and no rotation, use BitBlt instead (way faster). This is useful, for instance, for halo handles.
	We expect to replace them with vector graphics icons, anyway.

	But if drawing the world background, finding bounds is not needed. Use two BitBlts then:
	One to paint pixels, and a second one to set morphIds.
	In this way, performance is same as in HybridCanvas, and not too different from BitBltCanvas.
	This is especially important, the default World background is a Form!!"

	| p bits |

	"Special case for our world:
	- No bounds needed
	- currentTransformation is identity: No translation, no rotation, no scaling.
	BitBlt is enough, but we need two of them!!"
	currentMorph == world ifTrue: [
		p := (currentTransformation transform: aPoint) roundedHAFZ.
		auxBitBltEngine
			sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
			colorMap: (srcForm colormapIfNeededFor: form);
			clipRect: clipRect;
			sourceX: sourceRect left; sourceY: sourceRect top;
			destX: p x destY: p y width: sourceRect width height: sourceRect height;
			copyBits.
		bits := form bits.
		[
			form bits: engine morphIds.
			auxBitBltEngine
				sourceForm: nil;
				fillBitmap: (Bitmap with: (currentMorph morphId));
				combinationRule: `Form over`;
				colorMap: nil;
				copyBits.
		] ensure: [ form bits: bits ].
		^self ].

	"Otherwise, we need to compute bounds. While we are at it, set morphID for affected pixels."
	self fillRectangle: (sourceRect translatedBy: aPoint) color: `Color gray alpha: 0.01`.
	currentTransformation isPureTranslation
		ifTrue: [
			p := (currentTransformation transform: aPoint) roundedHAFZ.
			auxBitBltEngine
				sourceForm: srcForm; fillColor: nil; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				sourceX: sourceRect left; sourceY: sourceRect top;
				destX: p x destY: p y width: sourceRect width height: sourceRect height;
				copyBits ]
		ifFalse: [
			| bb cellSize dstBox srcBox srcQuad |
			cellSize := 4.
			srcBox := sourceRect.
			dstBox := (currentTransformation externalBoundingRectOf: srcBox)
				encompassingIntegerRectangle.
			srcQuad := dstBox corners collect: [ :pt | (currentTransformation inverseTransform: pt) roundedHAFZ ].
			dstBox := (currentTransformation externalBoundingRectOf: (srcBox translatedBy: aPoint-0.5))
				encompassingIntegerRectangle.
			bb := WarpBlt toForm: form.
			bb
				sourceForm: srcForm; combinationRule: Form blend;
				colorMap: (srcForm colormapIfNeededFor: form);
				clipRect: clipRect;
				cellSize: cellSize;
				copyQuad: srcQuad toRect: dstBox ].! !


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 3/12/2025 12:39:57'!
morphIdAt: aPoint
	| index |
	index := aPoint y * targetWidth + aPoint x + 1.
	(index between: 1 and: morphIds size) ifFalse: [ ^0 ].
	^morphIds at: index! !


!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:38:46'!
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
	alphaG := alphaR * fillColor alpha.
	alphaB := alphaR * fillColor alpha.

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
		morphIds at: pixelIndex put: currentMorphId ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:38:56'!
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
		morphIds at: pixelIndex put: currentMorphId ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:39:02'!
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
		morphIds at: pixelIndex put: currentMorphId ].! !


!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:39:08'!
blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| resultR resultG resultB antiAliasAlpha alpha unAlpha resultAlpha targetWord targetAlpha |

	antiAliasAlpha := antiAliasAlphaBits / 127.0.
	alpha := antiAliasAlpha * fillColor alpha.

	alpha = 0.0 ifFalse: [
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
		morphIds at: pixelIndex put: currentMorphId ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:39:15'!
blendStrokeAndFillAt: pixelIndex isInside: isInside antiAliasAlphaByte: strokeAntiAliasAlphaBits
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB strokeAAAlpha alpha unAlpha foreR foreG foreB targetAlpha resultAlpha |

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
		morphIds at: pixelIndex put: currentMorphId ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/12/2025 12:39:23'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alpha unAlpha resultAlpha targetAlpha |
	alpha := strokeAntiAliasAlphaBits / 127.0.
	alpha := alpha * strokeColor alpha.

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
		morphIds at: pixelIndex put: currentMorphId ].! !

