'From Cuis 6.0 [latest update: #5699] on 17 March 2023 at 6:26:57 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/17/2023 18:09:59'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| thisMorph thisMorphReportedDamage thisMorphDamage uncoveredDamage aPartWasFullyCovered nonCoveredParts |
	uncoveredDamage := aDamageRecorder damageReportedOther.

	"Remove any damage that is completely covered by another morph on top.
	Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		thisMorph := rootMorphs at: i.
		thisMorphDamage := nil.
		thisMorphReportedDamage := aDamageRecorder damageReportedFor: thisMorph.
		thisMorphReportedDamage ifNotNil: [
			"Iterate all morphs up to the one directly above morph"
			1 to: i-1 do: [ :j |
				(rootMorphs at: j) fullyCoveredRectangle ifNotNil: [ :coveringR |
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
		thisMorphDamage := nil.
		nonCoveredParts removeAll.
		1 to: uncoveredDamage size do: [ :j |
			(uncoveredDamage at: j) ifNotNil: [ :r |
				aPartWasFullyCovered := false.
				(rootMorphs at: i) on: r
					fullyCoveredRect: [ :fullyCoveredRect |
						thisMorphDamage
							ifNil: [thisMorphDamage := fullyCoveredRect]
							ifNotNil: [thisMorphDamage updateMerging: fullyCoveredRect].
						aPartWasFullyCovered := true. ]
					partiallyCoveredPartsDo: [ :partiallyCoveredRect|
						thisMorphDamage
							ifNil: [thisMorphDamage := partiallyCoveredRect]
							ifNotNil: [thisMorphDamage updateMerging: partiallyCoveredRect].
						aPartWasFullyCovered ifTrue: [
							nonCoveredParts add: partiallyCoveredRect ]]
					uncoveredRectsDo: [ :uncoveredRect |
						aPartWasFullyCovered ifTrue: [
							nonCoveredParts add: uncoveredRect ]].

				aPartWasFullyCovered ifTrue: [
					uncoveredDamage at: j put: nil ]]].
		uncoveredDamage addAll: nonCoveredParts.
		rootMorphsDamage at: i put: thisMorphDamage ].

	uncoveredDamage := uncoveredDamage select: [ :r | r notNil ].
	^ uncoveredDamage! !

