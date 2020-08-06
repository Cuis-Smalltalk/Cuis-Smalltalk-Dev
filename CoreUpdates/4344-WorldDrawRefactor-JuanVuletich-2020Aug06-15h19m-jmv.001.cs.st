'From Cuis 5.0 [latest update: #4343] on 6 August 2020 at 3:29:00 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/6/2020 15:22:52'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rootMorphs rootMorphsDamage allDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph ].

	rootMorphs _ aPasteUpMorph privateSubmorphs.
	rootMorphsDamage _ Array new: rootMorphs size.
	
	allDamage _ self drawWorldComputeDamage: aPasteUpMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage.

	"Draw World"
	allDamage do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	self drawWorld: aPasteUpMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage allDamage: allDamage.

	aDamageRecorder reset.
	^ allDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/6/2020 15:24:32'!
drawWorld: aPasteUpMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage allDamage: allDamage
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| morph morphBounds morphDamage |

	"Iterate from back to front."
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph _ rootMorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds.
			morphDamage _ rootMorphsDamage at: i.
			allDamage do: [ :r | | intersection |
				intersection _ r intersect: morphBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage _ intersection quickMerge: morphDamage ]].
			morphDamage ifNotNil: [
				self newClipRect: morphDamage.
				self fullDraw: morph.
				allDamage add: morphDamage ]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/6/2020 15:28:40'!
drawWorldComputeDamage: aPasteUpMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background."

	| allDamage morph morphBounds morphDamage oldMorphDamage reuse ri morphBefore |
	morphDamage _ OrderedCollection new.
	oldMorphDamage _ OrderedCollection new.
	allDamage _ aDamageRecorder damageReportedNotVisibleAndCarriedByHand.
	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds.
			morphDamage removeAll.
			(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r |
				morphDamage add: r ].
			allDamage do: [ :r |
				ri _ r intersect: morphBounds.
				ri hasPositiveExtent ifTrue: [
					morphDamage add: ri ]].
			1 to: i-1 do: [ :j |
				reuse _ oldMorphDamage.
				oldMorphDamage _ morphDamage.
				morphDamage _ reuse removeAll.
				morphBefore _ rootMorphs at: j.
				oldMorphDamage do: [ :r |
					morphBefore addPossiblyUncoveredAreasIn: r to: morphDamage ]].
			(Rectangle merging: morphDamage) ifNotNil: [ :morphDamageRect |
				rootMorphsDamage at: i put: morphDamageRect.
				morph addPossiblyUncoveredAreasIn: morphDamageRect to: allDamage ]]].

	^ allDamage! !

