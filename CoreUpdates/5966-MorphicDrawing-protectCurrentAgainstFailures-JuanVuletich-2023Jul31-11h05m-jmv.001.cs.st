'From Cuis 6.0 [latest update: #5964] on 31 July 2023 at 12:52:08 pm'!

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 7/31/2023 12:48:22'!
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
	self clippingByCurrentMorphDo: [
		currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

	(current postDrawOn: self) ifTrue: [
		self displayBoundsUpdateOnCurrentMorph ].

	current isHighlighted ifTrue: [
		self drawHighlight: current ].! !


!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 7/31/2023 12:49:32'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := true.
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


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/31/2023 12:50:23'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := true.
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


!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 7/31/2023 12:50:58'!
drawCurrentAndSubmorphs
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := true.
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

