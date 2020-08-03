'From Cuis 5.0 [latest update: #4276] on 22 July 2020 at 8:03:20 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 19:06:16'!
worldBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ Preferences haloEnclosesFullBounds
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/22/2020 19:56:15'!
drawCurrentAndSubmorphs

	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullDraw: clipped ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 20:02:11'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount |

"quizas llamarlo #findBoundsDo: o algo asi"
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
	].

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
					thisMorphBounds _ morph displayFullBounds ifNil: [10@10 extent: 10@10].
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

	"Draw morphs"
	aPasteUpMorph submorphsReverseDo: [ :morph | | morphBounds |
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds ifNil: [10@10 extent: 10@10].
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 19:58:44'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	| addMyBounds trySubmorphs currentMorphBounds |
	aMorph visible ifTrue: [
		addMyBounds _ aMorph valueOfProperty: #needsRedraw ifAbsent: [ false ].
		trySubmorphs _ aMorph valueOfProperty: #submorphNeedsRedraw ifAbsent: [ false ].
		(trySubmorphs or: [addMyBounds]) ifTrue: [
			self into: aMorph.
			addMyBounds ifTrue: [
				currentMorph setProperty: #needsRedraw toValue: false.
				currentMorph drawOn: self.
				currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
				currentMorph displayBounds: currentMorphBounds.
				aDamageRecorder recordInvalidRect: currentMorphBounds.
			].
			trySubmorphs ifTrue: [
				currentMorph setProperty: #submorphNeedsRedraw toValue: false.
				currentMorph clippedSubmorph ifNotNil: [ :clipped |
					self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
				currentMorph unclippedSubmorphsReverseDo: [ :m |
					self fullAddRedrawRect: m to: aDamageRecorder ]
			].
			self outOfMorph
		]]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences enable: #logDebuggerStackToFile.
Morph allSubInstancesDo: [ :m | m redrawNeeded ].
self runningWorld restoreDisplay; displayWorldSafely!