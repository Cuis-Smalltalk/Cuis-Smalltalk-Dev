'From Cuis 6.0 [latest update: #5843] on 9 June 2023 at 12:18:10 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/9/2023 11:56:14'!
drawRoots: rootMorphs rootsDamage: rootsDamage backgroundDamage: backgroundDamage
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."
	"Iterate from back to front."

	| morph morphDamage allDamage newDamageFromMorphsBelow |
	"Don't need a copy just because nobody will use it afterwards"
	newDamageFromMorphsBelow := backgroundDamage.
	allDamage := Rectangle merging: newDamageFromMorphsBelow.
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph := rootMorphs at: i.
		morphDamage := rootsDamage at: i.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection := r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage :=  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			newDamageFromMorphsBelow add: morphDamage.
			allDamage
				ifNil: [ allDamage := morphDamage copy ]
				ifNotNil: [ allDamage updateMerging: morphDamage ]]].
	^allDamage
"ESTE DEBERIA PODER SIMPLIFICARSE AHORA"! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/9/2023 11:55:04'!
gatherMorphDamage: rootMorphs rootsReportedDamage: rootsReportedDamage addDamageTo: allDamage
	"For each morph, remove any reported damage that is completely covered by another morph on top.
	Add any  areas that need to be redrawn to uncoveredDamage.
	uncoveredDamage will contain all the areas that need to be repaired. rootMorphsReportedDamage no longer relevant.
	Iterate from front to back"

	1 to: rootMorphs size do: [ :i | | thisMorphReportedDamage |
		thisMorphReportedDamage := rootsReportedDamage at: i.
		thisMorphReportedDamage ifNotNil: [
			"Iterate all morphs up to the one directly above us"
			1 to: i-1 do: [ :j |
				(self fullyCoveredRectangleOf: (rootMorphs at: j)) ifNotNil: [ :coveringR | | nonCoveredParts |
					nonCoveredParts := nil.
					1 to: thisMorphReportedDamage size do: [ :k |
						(thisMorphReportedDamage at: k) ifNotNil: [ :r |
							"If damage is completely covered, remove it. If it is partially covered, just keep uncovered parts."
							(coveringR intersects: r) ifTrue: [
								thisMorphReportedDamage at: k put: nil.
								(coveringR containsRect: r) ifFalse: [
									nonCoveredParts :=nonCoveredParts
										ifNil: [ r areasOutside: coveringR ]
										ifNotNil: [ nonCoveredParts, 	(			r areasOutside: coveringR) ]]]]].
					nonCoveredParts ifNotNil: [
						thisMorphReportedDamage addAll: nonCoveredParts ]]].
			"Store"
			thisMorphReportedDamage do: [ :r |
				r ifNotNil: [
					"DebuggingAid"
					"Display fill: r fillColor: Color random. (Delay forSeconds: 0.01) wait."
					allDamage add: r ]]]].

	"Remove redundant rectangles"
	1 to: allDamage size do: [ :i1 |
		(allDamage at: i1) ifNotNil: [ :r1 |
			1 to: allDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(allDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ 
								allDamage at: i2 put: nil ]]]]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/9/2023 11:54:08'!
scatterDamageTo: rootMorphs rootsDamageIn: rootsDamage allDamage: allDamage
	"uncoveredDamage was originally built possible without checking which areas are actually covered by some morph.
	Remove from uncoveredDamage any area that can be assigned to a morph.
	Give it to the topmost morph that can cover it.
	Answer uncoveredDamage, the areas where World background needs to be redrawn.
	Iterate from front to back"

	| unassignedDamage |
	unassignedDamage := allDamage copy.
	1 to: rootMorphs size do: [ :i | | thisMorph thisMorphDamageRect |
		thisMorph := rootMorphs at: i.
		thisMorphDamageRect := nil.
		thisMorph displayFullBounds ifNotNil: [ :thisMorphFullBounds | | addThisMorphDamageToUncoveredDamage |
			addThisMorphDamageToUncoveredDamage := false.
			1 to: unassignedDamage size do: [ :j |
				(unassignedDamage at: j) ifNotNil: [ :r |
					(r intersects: thisMorphFullBounds) ifTrue: [
						thisMorphDamageRect
							ifNil: [ thisMorphDamageRect := r intersect: thisMorphFullBounds ]
							ifNotNil: [ thisMorphDamageRect updateMerging: (r intersect: thisMorphFullBounds) ].
						(self fullyCoveredRectangleOf: thisMorph)
							ifNil: [ addThisMorphDamageToUncoveredDamage := true ]
							ifNotNil: [ :fullyCovered |
								r areasOutside: fullyCovered do: [ :notCovered | unassignedDamage add: notCovered ].
								unassignedDamage at: j put: nil ]]]].
			addThisMorphDamageToUncoveredDamage ifTrue: [
				unassignedDamage add: thisMorphDamageRect ].
		].
		rootsDamage at: i put: thisMorphDamageRect 
	].

	^unassignedDamage select: [ :r | r notNil ]! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 6/8/2023 16:13:01'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode := self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage := canvas drawWorld: self repair: damageRecorder.
	damageRecorder reset.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage := (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage := h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/9/2023 12:15:01'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage damage worldBackgroundRects |
	visibleRootMorphs := aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage := visibleRootMorphs collect: [ :m | aDamageRecorder damageReportedFor: m ].
	damage := aDamageRecorder damageReportedOther.
	self gatherMorphDamage: visibleRootMorphs rootsReportedDamage: visibleRootsDamage addDamageTo: damage.
	worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage.

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	^ self
		drawRoots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects.

	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"! !

!methodRemoval: MorphicCanvas #computeDamage:repair:rootMorphs:rootMorphsDamage: stamp: 'jmv 6/9/2023 12:17:44'!
MorphicCanvas removeSelector: #computeDamage:repair:rootMorphs:rootMorphsDamage:!
!methodRemoval: MorphicCanvas #drawWorld:rootMorphs:rootMorphsDamage:backgroundDamage: stamp: 'jmv 6/9/2023 12:17:44'!
MorphicCanvas removeSelector: #drawWorld:rootMorphs:rootMorphsDamage:backgroundDamage:!
