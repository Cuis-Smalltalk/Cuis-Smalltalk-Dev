'From Cuis7.3 [latest update: #7067] on 11 March 2025 at 12:18:33 pm'!

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 3/7/2025 10:58:53'!
drawCurrentAndSubmorphs
	| current |
	currentMorph visible ifFalse: [ ^self ].

	currentMorph drawOn: self.
	self displayBoundsSetOnCurrentMorph.

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')
	Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	currentMorph hasSubmorphs ifTrue: [
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	(current postDrawOn: self) ifTrue: [
		self displayBoundsUpdateOnCurrentMorph ].

	current isHighlighted ifTrue: [
		self drawHighlight: current ].! !


!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 3/7/2025 11:00:26'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				currentMorph submorphsMightProtrude ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourSetOnCurrentMorph.
		self contourUpdateOnAllOwners ].

	"Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(current postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph.
			self contourUpdateOnCurrentMorph.
			self contourUpdateOnAllOwners ].
		current isHighlighted ifTrue: [
			self drawHighlight: current ]].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 3/7/2025 11:00:33'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				currentMorph submorphsMightProtrude ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph ].

	"Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(current postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph ].
		current isHighlighted ifTrue: [
			self drawHighlight: current ]].! !


!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 3/7/2025 11:01:31'!
drawCurrentAndSubmorphs
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				currentMorph submorphsMightProtrude ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self onMorphIdsDo: [
			currentMorph drawOn: self ].
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph ].

	"Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(current postDrawOn: self) ifTrue: [
			self onMorphIdsDo: [
				current postDrawOn: self ].
			self displayBoundsUpdateOnCurrentMorph ].
		current isHighlighted ifTrue: [
			self drawHighlight: current ]].! !


!VectorEngine class methodsFor: 'instance creation' stamp: 'jmv 3/6/2025 15:43:47'!
onForm: aForm subPixelAntiAliasing: aBoolean
	"nil means use default kind of anti aliasing
	Use the VectorEnginePlugin if available."

	| engineClass doSubPixelAntiAliasing |
	self hasSubclasses
		ifFalse: [
			engineClass := self ]
		ifTrue: [
			doSubPixelAntiAliasing := aBoolean ifNil: [
				"Assume high resolution implies high DPI"
				Display width <= 1920 ].
			engineClass := VectorEngineWithPlugin isPluginAvailable
				ifTrue: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineWithPluginSubPixel]
					ifFalse: [VectorEngineWithPluginWholePixel]]
				ifFalse: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineSubPixel]
					ifFalse: [VectorEngineWholePixel]]].
		^engineClass new initializeOnForm: aForm.! !

