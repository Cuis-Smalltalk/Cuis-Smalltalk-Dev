'From Cuis 5.0 [latest update: #4272] on 22 July 2020 at 2:05:09 pm'!

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 7/22/2020 12:38:36'!
forceDamageToScreen: allDamage
	"Force all the damage rects to the screen."

	"allDamage do: [ :r | 
		self forceToScreen: r ]."
	"Do it at once. Otherwise, some flicking with 'broken' morphs was visible."
	(Rectangle merging: allDamage) ifNotNil: [ :r |
		self forceToScreen: r ]! !


!WorldState methodsFor: 'hands' stamp: 'jmv 7/22/2020 12:42:15'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result |
	result _ OrderedCollection new.
	hands do: [:hand |
		hand needsToBeDrawn ifTrue: [
			hand hasChanged
				ifTrue: [result add: hand]
				ifFalse: [
					hand morphFullBoundsInWorld ifNotNil: [ :handBounds |
						(damageList anySatisfy: [ :r | r intersects: handBounds]) ifTrue: [
							result add: hand]]]]].
	^ result
! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 12:36:25'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount morphBounds |

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
					thisMorphBounds _ morph morphFullBoundsInWorld.
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
	aPasteUpMorph submorphsReverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !

