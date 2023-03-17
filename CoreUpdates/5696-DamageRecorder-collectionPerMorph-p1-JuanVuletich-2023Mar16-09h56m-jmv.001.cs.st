'From Cuis 6.0 [latest update: #5695] on 16 March 2023 at 9:57:27 am'!

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 3/16/2023 09:56:24'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand.
	Answer might include nils. Skip them."
	| answer possiblyMoreRectsToMerge |
	answer := OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :rs | |rects|
			rects := rs isCollection ifTrue: [rs] ifFalse: [Array with:rs].
			rects do: [ :r |
				(m visible not or: [m owner isNil or: [m isOwnedByHand ]]) ifTrue: [
					answer add: r]]].
		otherDamage do: [ :r | answer add: r ].
	].
	possiblyMoreRectsToMerge := true.
	[possiblyMoreRectsToMerge] whileTrue: [
		possiblyMoreRectsToMerge := false.
		answer withIndexDo: [ :r1 :i |
			r1 ifNotNil: [
				i+1 to: answer size do: [ :j | | r2 |
					r2 := answer at: j.
					r2 ifNotNil: [
						(r1 intersects: r2) ifTrue: [
							r1 updateMerging: r2.
							answer at: j put: nil.
							possiblyMoreRectsToMerge := true ]]]]]].
	^answer! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/16/2023 09:48:11'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| uncoveredDamage additionalUncoveredDamage morph morphDamage morphFullBounds |
	uncoveredDamage := aDamageRecorder damageReportedOther.
	additionalUncoveredDamage := OrderedCollection new.

	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph := rootMorphs at: i.
		morphFullBounds := morph displayFullBounds.
		morphDamage := nil.
		"Reported damage can be ignored if the area fully covered by another morph above us."
		(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :rs | | rects wasFullyCovered |
			rects := rs isCollection ifTrue: [ rs ] ifFalse: [ Array with: rs ].
			rects do: [ :r |
				wasFullyCovered := false.
				1 to: i-1 do: [ :j |
					wasFullyCovered := wasFullyCovered or: [ (rootMorphs at: j) fullyCovers: r ]].
				wasFullyCovered ifFalse: [
					uncoveredDamage add: r ]]].
		
		"Any uncovered damage this morph may overlap must be redrawn by it."
		morphFullBounds ifNotNil: [
			uncoveredDamage withIndexDo: [ :r :j | | intersection |
				r ifNotNil: [
					intersection := r intersect: morphFullBounds.
					intersection hasPositiveExtent ifTrue: [
						r = intersection ifTrue: [ uncoveredDamage at: j put: nil ].
						morphDamage
							ifNil: [ morphDamage := intersection ]
							ifNotNil: [ morphDamage updateMerging: intersection ]]]]].
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

