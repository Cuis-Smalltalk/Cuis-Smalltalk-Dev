'From Cuis 5.0 [latest update: #4300] on 27 July 2020 at 1:26:06 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/27/2020 00:53:40'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb |
	savedPatch _ prevSavedPatch.
	(savedPatch isNil or: [savedPatch extent ~= aRectangle extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: aRectangle extent depth: form depth ].
	bb _ BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	savedPatch offset: aRectangle topLeft.
	^savedPatch! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/27/2020 01:24:01'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder."
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size = 0 ifTrue: [
		^rectsToRepair].

	rectsToRepair size > 1 ifTrue: [									"Or there is no point in attempting to merge them!!"
		previousRectsToRepair _ OrderedCollection new.

		"Until no more rectangles are merged, i.e. each morph intersects at most one rectangle."
		previousCount _ 0.
		[rectsToRepair size = previousCount] whileFalse: [
			previousCount _ rectsToRepair size.
			aPasteUpMorph submorphsReverseDo: [ :morph | | thisMorphRectangle thisMorphBounds |
				morph visible ifTrue: [
					reuse _ previousRectsToRepair.
					previousRectsToRepair _ rectsToRepair.
					rectsToRepair _ reuse removeAll.
					thisMorphRectangle _ nil.
					thisMorphBounds _ morph displayFullBounds ifNil: [0@0 extent: 10@10].
					previousRectsToRepair do: [ :r |
						(thisMorphBounds intersects: r)
							ifTrue: [ thisMorphRectangle _ r quickMerge: thisMorphRectangle ]
							ifFalse: [ rectsToRepair add: r ]].
					thisMorphRectangle
						ifNotNil: [rectsToRepair add: thisMorphRectangle]]]].
	].

	"Draw World"
	rectsToRepair do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Repair rects. Draw morphs"
	rectsToRepair do: [ :r |
		self newClipRect: r.
		aPasteUpMorph submorphsReverseDo: [ :morph | | morphBounds |
			morph visible ifTrue: [
				morphBounds _ morph displayFullBounds ifNil: [0@0 extent: 10@10].
				(morphBounds intersects: r) ifTrue: [
					self fullDraw: morph ]]]].

	^ rectsToRepair! !


"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences enable: #logDebuggerStackToFile.
Morph allSubInstancesDo: [ :m | m redrawNeeded ].
self runningWorld restoreDisplay; displayWorldSafely!
