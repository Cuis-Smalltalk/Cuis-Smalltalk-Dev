'From Cuis 6.0 [latest update: #5842] on 8 June 2023 at 12:11:52 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/8/2023 11:27:50'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| thisMorph thisMorphReportedDamage thisMorphDamage addThisMorphDamageToUncoveredDamage uncoveredDamage nonCoveredParts |
	uncoveredDamage := aDamageRecorder damageReportedOther.

	"Remove any damage that is completely covered by another morph on top.
	Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		thisMorph := rootMorphs at: i.
		thisMorphReportedDamage := aDamageRecorder damageReportedFor: thisMorph.
		thisMorphReportedDamage ifNotNil: [
			"Iterate all morphs up to the one directly above morph"
			1 to: i-1 do: [ :j |
				(self fullyCoveredRectangleOf: (rootMorphs at: j)) ifNotNil: [ :coveringR |
					nonCoveredParts := nil.
					1 to: thisMorphReportedDamage size do: [ :k |
						(thisMorphReportedDamage at: k) ifNotNil: [ :r |
							"If damage is completely covered, remove it. If it is partially covered, just keep uncovered parts."
							(coveringR intersects: r) ifTrue: [
								thisMorphReportedDamage at: k put: nil.
								(coveringR containsRect: r) ifFalse: [
									nonCoveredParts :=nonCoveredParts
										ifNil: [ r areasOutside: coveringR ]
										ifNotNil: [ nonCoveredParts, 	(			r areasOutside: coveringR) ]]]]].
					nonCoveredParts ifNotNil: [
						thisMorphReportedDamage addAll: nonCoveredParts ]]].
			"Store"
			thisMorphReportedDamage do: [ :r |
				r ifNotNil: [
					"DebuggingAid"
					"Display fill: r fillColor: Color random. (Delay forSeconds: 0.01) wait."
					uncoveredDamage add: r ]]]].

	"Remove redundant rectangles"
	1 to: uncoveredDamage size do: [ :i1 |
		(uncoveredDamage at: i1) ifNotNil: [ :r1 |
			1 to: uncoveredDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(uncoveredDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ 
								uncoveredDamage at: i2 put: nil ]]]]]].

	"Remove from uncoveredDamage any area that can be assigned to a morph.
	Give it to the topmost morph that can cover it.
	Iterate from front to back"
	nonCoveredParts := OrderedCollection new.
	1 to: rootMorphs size do: [ :i |
		thisMorph := rootMorphs at: i.
		thisMorph displayFullBounds ifNotNil: [ :thisMorphSoonFullBounds |
			thisMorphDamage := nil.
			addThisMorphDamageToUncoveredDamage := false.
			nonCoveredParts removeAll.
			1 to: uncoveredDamage size do: [ :j |
				(uncoveredDamage at: j) ifNotNil: [ :r |
					(r intersects: thisMorphSoonFullBounds) ifTrue: [
						thisMorphDamage
							ifNil: [ thisMorphDamage := r intersect: thisMorphSoonFullBounds ]
							ifNotNil: [ thisMorphDamage updateMerging: (r intersect: thisMorphSoonFullBounds) ].
						(self fullyCoveredRectangleOf: thisMorph)
							ifNil: [ addThisMorphDamageToUncoveredDamage := true ]
							ifNotNil: [ :fullyCovered |
								r areasOutside: fullyCovered do: [ :notCovered | nonCoveredParts add: notCovered ].
								uncoveredDamage at: j put: nil ]]]].
			uncoveredDamage addAll: nonCoveredParts.
			addThisMorphDamageToUncoveredDamage ifTrue: [
				uncoveredDamage add: thisMorphDamage ].
			rootMorphsDamage at: i put: thisMorphDamage 
		]].

	uncoveredDamage := uncoveredDamage select: [ :r | r notNil ].
	^ uncoveredDamage! !

