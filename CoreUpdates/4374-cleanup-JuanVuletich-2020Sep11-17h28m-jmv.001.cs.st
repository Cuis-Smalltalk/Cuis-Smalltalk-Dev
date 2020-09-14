'From Cuis 5.0 [latest update: #4373] on 11 September 2020 at 5:35:59 pm'!

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 9/11/2020 17:30:25'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand."
	| answer |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m isNil or: [
				m visible not or: [m owner isNil or: [m owner is: #HandMorph]]]) ifTrue: [
					answer add: r]]].
	^answer! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/11/2020 17:34:47'!
computeDamage: aPasteUpMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background."

	| uncoveredDamage morph morphDamage prevMorphDamage reuseInstance ri morphBefore |
	morphDamage _ OrderedCollection new.
	prevMorphDamage _ OrderedCollection new.
	uncoveredDamage _ aDamageRecorder damageReportedOther.
	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morph displayFullBounds ifNotNil: [ :morphBounds |
			morphDamage removeAll.
			(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r |
				morphDamage add: r ].
			uncoveredDamage do: [ :r |
				ri _ r intersect: morphBounds.
				ri hasPositiveExtent ifTrue: [
					morphDamage add: ri ]].
			1 to: i-1 do: [ :j |
				reuseInstance _ prevMorphDamage.
				prevMorphDamage _ morphDamage.
				morphDamage _ reuseInstance removeAll.
				morphBefore _ rootMorphs at: j.
				prevMorphDamage do: [ :r |
					morphBefore addPossiblyUncoveredAreasIn: r to: morphDamage ]].
			(Rectangle merging: morphDamage) ifNotNil: [ :morphDamageRect |
				rootMorphsDamage at: i put: morphDamageRect.
				morph addPossiblyUncoveredAreasIn: morphDamageRect to: uncoveredDamage ]]].

	^ uncoveredDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/11/2020 16:37:58'!
drawWorldBackground: aPasteUpMorph rects: worldBackgroundDamage
	"Draw worldBackgroundDamage rects for aPasteUpMorph.
	Do not include submorphs."

	worldBackgroundDamage do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/11/2020 17:24:04'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rootMorphs rootMorphsDamage worldDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph ].

	rootMorphs _ aPasteUpMorph privateSubmorphs.
	rootMorphsDamage _ Array new: rootMorphs size.
	
	worldDamage _ self computeDamage: aPasteUpMorph repair: aDamageRecorder
		rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage.

	self drawWorldBackground: aPasteUpMorph rects: worldDamage.

	self drawWorld: aPasteUpMorph
		rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
		allDamage: worldDamage.

	aDamageRecorder reset.
	^ worldDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/11/2020 17:32:32'!
drawWorld: aPasteUpMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage allDamage: worldDamage
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| morph morphBounds morphDamage |

	"Iterate from back to front."
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph _ rootMorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds.
			morphDamage _ rootMorphsDamage at: i.
			worldDamage do: [ :r | | intersection |
				intersection _ r intersect: morphBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage _ intersection quickMerge: morphDamage ]].
			morphDamage ifNotNil: [
				self newClipRect: morphDamage.
				self fullDraw: morph.
				worldDamage add: morphDamage ]]].! !

!methodRemoval: MorphicCanvas #drawWorld:rects: stamp: 'jmv 9/11/2020 17:28:24'!
MorphicCanvas removeSelector: #drawWorld:rects:!
!methodRemoval: MorphicCanvas #drawWorldComputeDamage:repair:rootMorphs:rootMorphsDamage: stamp: 'jmv 9/11/2020 17:28:51'!
MorphicCanvas removeSelector: #drawWorldComputeDamage:repair:rootMorphs:rootMorphsDamage:!
!methodRemoval: DamageRecorder #damageReportedNotVisibleAndCarriedByHand stamp: 'jmv 9/11/2020 17:30:34'!
DamageRecorder removeSelector: #damageReportedNotVisibleAndCarriedByHand!
