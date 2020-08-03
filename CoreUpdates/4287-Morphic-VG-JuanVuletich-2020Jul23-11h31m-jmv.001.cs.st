'From Cuis 5.0 [latest update: #4283] on 23 July 2020 at 11:32:30 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/23/2020 11:17:01'!
updateDisplayBounds: aMorph
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aMorph visible ifTrue: [
		aMorph displayBounds ifNil: [
			self into: aMorph.
			currentMorph drawOn: self.
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw.
			currentMorph clippedSubmorph ifNotNil: [ :clipped |
				self clippingByCurrentMorphDo: [ self updateDisplayBounds: clipped ]].
			currentMorph unclippedSubmorphsReverseDo: [ :m |
				self updateDisplayBounds: m ].
			self outOfMorph
		]]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/23/2020 11:26:42'!
updateHandsDisplayBounds: aPasteUpMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aPasteUpMorph handsDo: [ :hand |
		self into: hand.
		hand submorphsDo: [ :morph |
			self updateDisplayBounds: morph ].
		self outOfMorph ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:10:46'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world)."

	| answer |
	answer _ self displayBounds.
	answer isNil ifTrue: [ ^nil ].
	self submorphsDrawingOutsideReverseDo: [ :m |
		answer _ answer quickMerge: m displayFullBounds ].
	^answer! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 11:32:11'!
submorphsDisplayFullBounds
	"Current full bounds of submorphs carried by the hand.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	submorphs isEmpty ifTrue: [ ^nil ].
	fullBounds _ nil.
	(self valueOfProperty: #lastPos)
		ifNil: [						"Just once when a new morph (not in the world) is attached to the hand."
			self submorphsDo: [ :m |
				m displayFullBounds ifNotNil: [ :b |
					fullBounds _ fullBounds ifNil: [b] ifNotNil: [ fullBounds merge: b ]]]]
		ifNotNil: [ :lastPos |						"When carrying morphs around."
			self submorphsDo: [ :m |
				m displayFullBounds ifNotNil: [ :b |
					fullBounds _ fullBounds ifNil: [b] ifNotNil: [ fullBounds merge: b ]]].
			fullBounds _ fullBounds translatedBy: self morphPosition-lastPos ].
	^fullBounds! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/23/2020 11:26:59'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount |

"quizas llamarlo #findBoundsDo: o algo asi"
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph.
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

