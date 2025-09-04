'From Cuis7.5 [latest update: #7478] on 2 September 2025 at 12:37:34 pm'!

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 9/2/2025 12:36:44'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifFalse: [
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


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 9/2/2025 12:36:50'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifFalse: [
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


!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 9/2/2025 12:36:56'!
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
				(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifFalse: [
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

