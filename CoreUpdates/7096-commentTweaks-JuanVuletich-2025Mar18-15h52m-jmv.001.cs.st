'From Cuis7.3 [latest update: #7086] on 18 March 2025 at 3:57:36 pm'!

!VectorEngineWithPluginWholePixel commentStamp: 'jmv 3/18/2025 15:52:48' prior: 0!
Does only whole pixel antialiasing. Appropriate for very high resolution displays, or slower systems where the extra computational load of VectorEngineWithPluginSubPixel is not affordable.
Variables edgeCounts and alphaMask are single byte per pixel.

Uses VectorEnginePlugin for performance.

Note: As a performance optimization, target is opaque. This means that any translucency (alpha channel) in targetForm is ignored on input, and set to opaque (255) on the primBlend* operations. The 'WPOT' suffix in these primitives means 'Whole Pixel AA, Opaque Target'.

If you need target translucency computed correctly, please use VectorEngineWithPlginSubPixel (maybe setting subPixelDelta to 0.0, to effectively disable subpixel rasterization).!


!Morph methodsFor: 'drawing' stamp: 'jmv 3/18/2025 15:56:50'!
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
		"Ask for a Canvas with subpixels so it can also handle translucent target.
		This is relevant if #isPluginAvailable is true, and we're using the plugin, as the Smalltalk engine does proper
		target translucency both in subPixel and wholePixel AA variants."
		auxCanvas := VectorCanvas onFormWithSubPixelAntiAliasing: answer.
		auxCanvas geometryTransformation: fauxLocation.
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 3/18/2025 15:57:06'!
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
		"Ask for a Canvas with subpixels so it can also handle translucent target.
		This is relevant if #isPluginAvailable is true, and we're using the plugin, as the Smalltalk engine does proper
		target translucency both in subPixel and wholePixel AA variants."
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

