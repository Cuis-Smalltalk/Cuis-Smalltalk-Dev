'From Cuis 5.0 [latest update: #4337] on 5 August 2020 at 11:37:36 am'!

!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2020 11:34:38'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean
	Store value of 'hidden', because flags default to false."

	self visible == aBoolean
		ifTrue: [ ^ self ].
	self redrawNeeded.
	self privateFlagAt: 3 put: aBoolean not.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/5/2020 11:27:12'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
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
	allDamage _ aDamageRecorder damageReportedNotVisibleAndCarriedByHand.

	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i | | morph morphBounds morphDamage |
		morph _ rootMorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds.
			morphDamage _ OrderedCollection new.
			(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r |
				morphDamage add: r ].
			allDamage do: [ :r | | ri |
				ri _ r intersect: morphBounds.
				ri hasPositiveExtent ifTrue: [
					morphDamage add: ri ]].
			1 to: i-1 do: [ :j | | morphBefore oldMorphDamage |
				oldMorphDamage _ morphDamage.
				morphDamage _ OrderedCollection new.
				morphBefore _ rootMorphs at: j.
				oldMorphDamage do: [ :r |
					morphBefore addPossiblyUncoveredAreasIn: r to: morphDamage ]].
			(Rectangle merging: morphDamage) ifNotNil: [ :morphDamageRect |
				rootMorphsDamage at: i put: morphDamageRect.
				morph addPossiblyUncoveredAreasIn: morphDamageRect to: allDamage ]]].

	"Draw World"
	allDamage do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Iterate from back to front."
	rootMorphs size to: 1 by: -1 do: [ :i | | morph morphBounds morphDamage |
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
				allDamage add: morphDamage ]]].

	aDamageRecorder reset.
	^ allDamage! !

