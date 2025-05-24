'From Cuis7.3 [latest update: #7081] on 18 March 2025 at 5:04:24 pm'!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId currentClipsSubmorphs clipCurrentMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 3/11/2025 16:09:44'!
invalidateDisplayRect: damageRect for: aMorph
	"
	If we clip submorphs, then we clip damageRect.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |
	self visible ifFalse: [ ^self].

	clippedRect := damageRect.
	aMorph == self ifFalse: [
		self clipsSubmorphs ifTrue: [
			b := self displayBounds.
			b ifNil: [ ^self ].
			clippedRect := damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect for: aMorph ].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/11/2025 16:09:41'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Integer pixel coordinates!!"

	| answer |
	answer := self basicDisplayBounds.
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsDo: [ :m |
			answer := answer
				ifNil: [m displayFullBounds]
				ifNotNil: [answer quickMerge: m displayFullBounds]]].
	^answer! !

!Morph methodsFor: 'updating' stamp: 'jmv 3/11/2025 16:09:48'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way.
	Notes:
		Area used before any change is reported here.
		Area used after any changes is reported when displayBounds are updated.
			See updateBoundsIn:addDamageTo: and senders.
	"

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self basicDisplayBounds ifNotNil: [ :b |
		self invalidateDisplayRect: b for: self ].
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self haloRedrawMayBeNeeded.
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 3/11/2025 16:09:53'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphs ifFalse: [ ^aBlock value ].

	prevClipRect := clipRect.
	prevClippingMorphRect := clippingMorphDisplayBounds.
	clippingMorphDisplayBounds := self boundingRectOfCurrentMorphAfterDraw insetBy: 1.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds := prevClippingMorphRect ]! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 3/11/2025 16:09:57'!
fullUpdateCurrentProtrudingBounds
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	currentMorph knowsOwnLocalBounds
		ifTrue: [self displayBoundsSetOnCurrentMorph]
		ifFalse: [
			currentMorph isKnownFailing
				ifTrue: [
					self drawCurrentAsError ]
				ifFalse: [
					"In these cases, #drawOn: is required to find bounds.
					#displayBoundsSetFrom: will use the bounds found by us in these case.
					See #displayBoundsSetFrom:"
					currentMorph drawOn: self.
					self displayBoundsSetOnCurrentMorph.
					(currentMorph postDrawOn: self) ifTrue: [
						self displayBoundsUpdateOnCurrentMorph ].
					(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifTrue: [
						currentMorph submorphsDo: [ :m |
							self fullUpdateProtrudingBounds: m ]]]].! !


!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/10/2025 10:24:22'!
currentMorph: aMorph
	aMorph
		ifNil: [
			currentMorphId := 0 ]
		ifNotNil: [
			currentMorphId := aMorph morphId ].! !


!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:51:04'!
blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: antiAliasAlphasWord
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha |

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
		morphIds at: pixelIndex put: currentMorphId<<8 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:52:06'!
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
		morphIds at: pixelIndex put: currentMorphId<<8 ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:52:26'!
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
		morphIds at: pixelIndex put: currentMorphId<<8 ].! !


!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:52:37'!
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
		morphIds at: pixelIndex put: currentMorphId<<8 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:52:51'!
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
		morphIds at: pixelIndex put: currentMorphId<<8 ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 3/18/2025 16:53:03'!
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
		morphIds at: pixelIndex put: currentMorphId<<8 ].! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 3/11/2025 15:48:09'!
initializePath

	super initializePath.

	"Set all plugin state. There might be several instances of this class, all using the same plugin.
	Only one of them will use the plugin during a path draw operation. See class variable PluginAccess."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat
		hopLength: hop asFloat.
	self primClipLeft: clipLeft clipTop: clipTop clipRight: clipRight clipBottom: clipBottom.
	self primCurrentMorphId: currentMorphId currentClipsSubmorphs: false.
	self primClipCurrentMorph: false morphIds: morphIds.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.
	self primStrokeWidth: strokeWidth.
	self primStrokeColor: strokeColor.
	self primFillColor: fillColor.

	self primInitializePath.! !

!methodRemoval: VectorEngineSmalltalk #clipCurrentMorph: stamp: 'jmv 3/18/2025 17:02:19'!
VectorEngineSmalltalk removeSelector: #clipCurrentMorph:!
!methodRemoval: VectorEngineDrawer #clipCurrentMorph: stamp: 'jmv 3/18/2025 17:02:20'!
VectorEngineDrawer removeSelector: #clipCurrentMorph:!
!methodRemoval: VectorEngine #clipCurrentMorph: stamp: 'jmv 3/18/2025 17:02:21'!
VectorEngine removeSelector: #clipCurrentMorph:!
!methodRemoval: VectorCanvas #clipCurrentMorph: stamp: 'jmv 3/18/2025 17:02:04'!
VectorCanvas removeSelector: #clipCurrentMorph:!
!methodRemoval: Morph #clipsSubmorphsReally stamp: 'jmv 3/18/2025 16:59:35'!
Morph removeSelector: #clipsSubmorphsReally!
!classDefinition: #VectorEngineDrawer category: #'Morphic-Support'!
VectorEngine subclass: #VectorEngineDrawer
	instanceVariableNames: 'targetForm targetWidth targetHeight morphIds edgeCounts alphaMask contour strokeColor strokeDashLength strokeDashArray strokeDashArrayLast strokeDashArrayOffset fillColor clipLeft clipTop clipRight clipBottom currentMorphId'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
