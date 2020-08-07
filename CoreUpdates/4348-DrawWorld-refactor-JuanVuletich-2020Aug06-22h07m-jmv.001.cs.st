'From Cuis 5.0 [latest update: #4347] on 6 August 2020 at 10:10:41 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/6/2020 22:08:50'!
drawWorld: aPasteUpMorph rects: allDamage
	"Draw allDamage rects for aPasteUpMorph"

	allDamage do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/6/2020 22:09:08'!
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
	
	allDamage _ self drawWorldComputeDamage: aPasteUpMorph repair: aDamageRecorder
		rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage.

	self drawWorld: aPasteUpMorph rects: allDamage.

	self drawWorld: aPasteUpMorph rootMorphs: rootMorphs
		rootMorphsDamage: rootMorphsDamage allDamage: allDamage.

	aDamageRecorder reset.
	^ allDamage! !

