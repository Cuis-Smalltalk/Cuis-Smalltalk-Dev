'From Cuis7.5 [latest update: #7431] on 23 August 2025 at 11:12:49 am'!

!Morph methodsFor: 'drawing' stamp: 'jmv 8/23/2025 11:09:35'!
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
	answer := auxCanvas form.
	"answer divideByAlpha."
	"answer makeAllPixelsOpaque."
	answer fixAlpha.
	extentOrNil ifNotNil: [ answer := answer magnifyTo: extentOrNil ].
	^answer! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/23/2025 11:12:27'!
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
		auxCanvas := VectorCanvas onForm: answer.
		auxCanvas engine enableTargetTranslucency.
		auxCanvas geometryTransformation: fauxLocation.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		[auxCanvas fullDraw: self] on: Error do: [ auxCanvas canvasToUse drawCurrentAsOutline ].
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !

