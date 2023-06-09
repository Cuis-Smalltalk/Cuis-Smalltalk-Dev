'From Cuis 6.0 [latest update: #5844] on 9 June 2023 at 12:50:02 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/9/2023 12:46:43'!
updateAllDamage: allDamage roots: rootMorphs rootsDamage: rootsDamage backgroundDamage: backgroundDamage
	"Detect any morph whose damage would grow because of morphs beneath it.
	If one found, add the new larger damage to allDamage and exit.
	See sender."
	"Iterate from back to front."

	| morph morphPreviousDamage morphDamage newDamageFromMorphsBelow |
	newDamageFromMorphsBelow := backgroundDamage copy.
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph := rootMorphs at: i.
		morphPreviousDamage := rootsDamage at: i.
		morphDamage := morphPreviousDamage copy.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection := r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage :=  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			newDamageFromMorphsBelow add: morphDamage ].
		morphDamage = morphPreviousDamage ifFalse: [
			allDamage add: morphDamage.
			^true 	]].
	^false! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/9/2023 12:48:37'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage damage worldBackgroundRects loopCount |
	visibleRootMorphs := aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage := visibleRootMorphs collect: [ :m | aDamageRecorder damageReportedFor: m ].
	damage := aDamageRecorder damageReportedOther.
	self gatherMorphDamage: visibleRootMorphs rootsReportedDamage: visibleRootsDamage addDamageTo: damage.
	worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage.
	"It is possible that some morph's damage gets enlarged due to morphs beneath it. If so, act as if the updated morph damage was requested damage from the start, and retry.
	Ocassionally this seemed to loop without bounds. Limit the number of retries."
	loopCount := 0.
	[ (self updateAllDamage: damage roots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects) and: [loopCount < 10] ] whileTrue: [
		loopCount := loopCount + 1.
		worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage ].

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	^ self
		drawRoots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects.

	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"! !

