'From Cuis7.3 [latest update: #7146] on 12 May 2025 at 4:20:07 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 5/12/2025 15:38:41'!
drawingFails
	self world ifNotNil: [ :w | w addKnownFailing: self ].
	self redrawNeeded.! !

!Morph methodsFor: 'drawing' stamp: 'jmv 5/12/2025 15:44:56'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas bounds |
	self ensureMinimumExtent.

	self requiresVectorCanvas ifTrue: [	
		^ self imageFormVG: extentOrNil depth: depth ].

	bounds := self fullBoundsInOwner.
	answerExtent := bounds extent.
	extentOrNil ifNotNil: [ answerExtent := answerExtent min: extentOrNil * 4 ].
	auxCanvas := MorphicCanvas depth: depth over: (bounds origin extent: answerExtent ceiling).
	[auxCanvas fullDraw: self] on: Error do: [ auxCanvas canvasToUse drawCurrentAsOutline ].
	answer := auxCanvas form divideByAlpha.
	extentOrNil ifNotNil: [ answer := answer magnifyTo: extentOrNil ].
	^answer! !

!Morph methodsFor: 'drawing' stamp: 'jmv 5/12/2025 15:45:08'!
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
		[auxCanvas fullDraw: self] on: Error do: [ auxCanvas canvasToUse drawCurrentAsOutline ].
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !

