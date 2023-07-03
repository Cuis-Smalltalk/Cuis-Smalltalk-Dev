'From Cuis 6.0 [latest update: #5886] on 3 July 2023 at 11:04:26 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/3/2023 11:03:17'!
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
	^allDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/3/2023 10:27:42'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage damage worldBackgroundRects loopCount |
	visibleRootMorphs := aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage := visibleRootMorphs collect: [ :m | aDamageRecorder damageReportedFor: m ].
	damage := aDamageRecorder damageReportedOther.
	aDamageRecorder reset.
	self gatherMorphDamage: visibleRootMorphs rootsReportedDamage: visibleRootsDamage addDamageTo: damage.
	worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage.
	"It is possible that some morph's damage gets enlarged due to morphs beneath it.
	If so, act as if 	the updated morph damage was requested damage from the start, and retry.
	Ocassionally this seemed to loop without bounds. Limit the number of retries."
	loopCount := 0.
	[ (self updateAllDamage: damage roots: visibleRootMorphs rootsDamage: visibleRootsDamage 
					backgroundDamage: worldBackgroundRects) and: [loopCount < 10] ] whileTrue: [
		loopCount := loopCount + 1.
		worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage ].

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	^ self drawRoots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects.

	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"! !

