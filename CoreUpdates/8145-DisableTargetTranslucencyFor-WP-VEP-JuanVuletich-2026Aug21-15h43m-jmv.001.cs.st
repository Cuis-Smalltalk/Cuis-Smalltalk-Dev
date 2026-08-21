'From Cuis7.9 [latest update: #8144] on 21 August 2026 at 3:57:11 pm'!

!VectorEngineWithPluginWholePixel methodsFor: 'accessing' stamp: 'jmv 21/Aug/2026, 6:45:47 pm (UTC)'!
enableTargetTranslucency
	"Compute correct target transucency.
	Not supported in the VM Plugin for the Whole Pixel Anti Aliasing Vector Graphics Engine.
	Optimized out for performance."

	self error: 'Not supported by the Whole Pixel Anti Aliasing Vector Graphics Engine Plugin.'.
! !


!Morph methodsFor: 'drawing' stamp: 'jmv 21/Aug/2026, 6:55:38 pm (UTC)'!
imageFormVG: extentOrNil depth: depth
	"Draw using a VectorCanvas
	Scale as needed. Draw unrotated."

	| prevNotVisibleFlag bounds extent fauxLocation answer auxCanvas |
	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	[
		bounds := self findFullBoundsInOwner.
		extentOrNil
			ifNotNil: [ | scale |
				extent := extentOrNil.
				scale := extent x asFloat / bounds width min: extent y asFloat / bounds height.
				fauxLocation := AffineTransformation withScale: scale position: bounds origin negated +1 * scale ]
			ifNil: [
				extent := bounds extent.
				fauxLocation := MorphicTranslation withTranslation: bounds origin negated ].
		answer := Form extent: extent depth: 32.
		"Ask for a Canvas with subpixels so it can also handle translucent target"
		auxCanvas := VectorCanvas onFormWithSubPixelAntiAliasing: answer .
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		auxCanvas geometryTransformation: fauxLocation.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		[auxCanvas fullDraw: self] on: Error do: [auxCanvas form fillColor: Color red].
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 21/Aug/2026, 6:53:00 pm (UTC)'!
bitMask
	"Answer an 1bpp Form with our bitMask.
	Include all pixels drawn by any morph in our submorphs tree.
	Query with #pixelValueAt:
	Pixels drawn by us (or any submorph) are 0. Pixels not drawn by us (or any submorph) are 1.
	Remember to do `self removeProperty: #bitMask when appropriate!!
	(i.e. when we change in such a way to make the bitMask invalid)
	
	Also see senders of #contour"

	| prevNotVisibleFlag bounds answer auxCanvas |

	(self valueOfProperty: #bitMask) ifNotNil: [ :bitMask |
		^bitMask ].

	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	bounds := self findFullBoundsInOwner.
	[
		answer := Form extent: bounds extent depth: 32.
		"Ask for a Canvas with subpixels so it can also handle translucent target"
		auxCanvas := VectorCanvas onFormWithSubPixelAntiAliasing: answer translation: bounds origin negated.
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^self setProperty: #bitMask toValue:
		"Reverse it so we get bit=1 for affected pixels."
		((answer asGrayForm: 1) asFormOfDepth: 1) reverse.! !


!VectorEngineWithPluginWholePixel reorganize!
('accessing' enableTargetTranslucency)
('plugin calls' primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin: primBlendFillOnlyLeft:top:right:bottom: primBlendStrokeAndFillLeft:top:right:bottom: primBlendStrokeOnlyLeft:top:right:bottom: primCubicBezierFromX:y:toX:y:control1X:y:control2X:y: primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes: primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes: primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes: primLineFromX:y:toX:y: primPathSequence:size: primQuadraticBezierFromX:y:toX:y:controlX:y: primTargetBits:morphIds:edgeCounts:alphaMask:affectedBits:contour:targetWidth:targetHeight:)
('private' auxPixelDataClass)
!

