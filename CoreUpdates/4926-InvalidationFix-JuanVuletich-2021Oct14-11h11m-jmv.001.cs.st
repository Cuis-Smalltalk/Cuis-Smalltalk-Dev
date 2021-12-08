'From Cuis 5.0 [latest update: #4923] on 14 October 2021 at 11:12:23 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/14/2021 11:12:04'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| uncoveredDamage additionalUncoveredDamage morph morphDamage morphFullBounds |
	uncoveredDamage _ aDamageRecorder damageReportedOther.
	additionalUncoveredDamage _ OrderedCollection new.

	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morphFullBounds _ morph displayFullBounds.
		morphDamage _ nil.
		"Reported damage can be ignored if the area fully covered by another morph above us."
		(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r | | wasFullyCovered |
			wasFullyCovered _ false.
			1 to: i-1 do: [ :j |
				wasFullyCovered _ wasFullyCovered or: [ (rootMorphs at: j) fullyCovers: r ]].
			wasFullyCovered ifFalse: [
				uncoveredDamage add: r ]].
		"Any uncovered damage this morph may overlap must be redrawn by it."
		uncoveredDamage withIndexDo: [ :r :j | | intersection |
			r ifNotNil: [
				intersection _ r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					r = intersection ifTrue: [ uncoveredDamage at: j put: nil ].
					morphDamage
						ifNil: [ morphDamage _ intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		rootMorphsDamage at: i put: morphDamage.
		uncoveredDamage add: morphDamage.
		"Whatever we cover completely is no longer uncoveredDamage"
		uncoveredDamage withIndexDo: [ :r :ri |
			r ifNotNil: [
				morph
					on: r
					ifCovered: [uncoveredDamage at: ri put: nil]
						uncoveredPartsDo: [ :r2 | additionalUncoveredDamage add: r2 ]
					else: []]].
		uncoveredDamage addAll: additionalUncoveredDamage.
		additionalUncoveredDamage removeAll.
	].

	"Remove redundant rectangles"
	1 to: uncoveredDamage size do: [ :i1 |
		(uncoveredDamage at: i1) ifNotNil: [ :r1 |
			1 to: uncoveredDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(uncoveredDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ uncoveredDamage at: i2 put: nil ]]]]]].

	"Answer any yet uncovered areas. World background should be drawn on them."
	^ uncoveredDamage select: [ :r | r notNil ].! !

