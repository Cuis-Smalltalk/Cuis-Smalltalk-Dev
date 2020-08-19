'From Cuis 5.0 [latest update: #4353] on 19 August 2020 at 9:30:04 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/19/2020 09:25:26'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world)."

	| answer |
	answer _ self displayBounds.
	answer ifNil: [ ^nil ].
	self submorphsDrawingOutsideReverseDo: [ :m |
		answer _ answer quickMerge: m displayFullBounds ].
	^answer! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/19/2020 09:27:55'!
drawWorldComputeDamage: aPasteUpMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background."

	| allDamage morph morphDamage oldMorphDamage reuse ri morphBefore |
	morphDamage _ OrderedCollection new.
	oldMorphDamage _ OrderedCollection new.
	allDamage _ aDamageRecorder damageReportedNotVisibleAndCarriedByHand.
	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morph displayFullBounds ifNotNil: [ :morphBounds |
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

