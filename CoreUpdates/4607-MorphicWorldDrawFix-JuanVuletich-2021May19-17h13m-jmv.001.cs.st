'From Cuis 5.0 [latest update: #4605] on 19 May 2021 at 5:14:29 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/19/2021 17:11:36'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage worldDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self updatingMorphBoundsDo: [
		aWorldMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aWorldMorph ].

	visibleRootMorphs _ aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage _ Array new: visibleRootMorphs size.
	
	worldDamage _ self computeDamage: aWorldMorph repair: aDamageRecorder
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage.

	self drawWorldBackground: aWorldMorph rects: worldDamage.

	self drawWorld: aWorldMorph
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage
		allDamage: worldDamage.

	aDamageRecorder reset.
	^ worldDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/19/2021 17:10:47'!
drawWorld: aWorldMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage allDamage: worldDamage
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| morph morphDamage |

	"Iterate from back to front."
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph _ rootMorphs at: i.
		morphDamage _ rootMorphsDamage at: i.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			worldDamage do: [ :r | | intersection |
				intersection _ r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage _ intersection quickMerge: morphDamage ]]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			worldDamage add: morphDamage ]].! !

