'From Cuis 6.0 [latest update: #5705] on 20 March 2023 at 5:34:10 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 10:24:08'!
fullyCoveredRectangle
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	Subclasses may implement smarter behavior, allowing for optimizations of update operations.
	The answer should be the largest fully covered rectangle we can know cheaply."

	^nil! !


!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/17/2023 10:57:18'!
fullyCoveredRectangle
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	This implementation is a good optimization for many common cases."

	| b e |
	"If we can't know better."
	self isOrAnyOwnerIsRotated ifTrue: [
		^ nil ].
	color mightBeTranslucent ifTrue: [
		^ nil ].
	b := self displayBounds.
	b isNil ifTrue: [
		^nil ].

	e := Theme current roundWindowCorners
		ifFalse: [
			"Due to possible anti aliasing, and possible conversion to integer, we can't
			really be sure 	about the 2 outer pixels at each edge."
			`2@2` ]
		ifTrue: [
			"Add 2 pixels in y direction for anti aliasing and rounding."
			(self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling+2 ].
	^ b insetBy: e.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/20/2023 17:31:53'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| thisMorph thisMorphReportedDamage thisMorphDamage uncoveredDamage nonCoveredParts |
	uncoveredDamage := aDamageRecorder damageReportedOther.

	"Remove any damage that is completely covered by another morph on top.
	Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		thisMorph := rootMorphs at: i.
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
		thisMorph := rootMorphs at: i.
		thisMorphDamage := nil.
		thisMorph displayFullBounds ifNotNil: [ :thisMorphSoonFullBounds |
			nonCoveredParts removeAll.
			1 to: uncoveredDamage size do: [ :j |
				(uncoveredDamage at: j) ifNotNil: [ :r |
					(r intersects: thisMorphSoonFullBounds) ifTrue: [
						thisMorphDamage
							ifNil: [ thisMorphDamage := r intersect: thisMorphSoonFullBounds ]
							ifNotNil: [ thisMorphDamage updateMerging: (r intersect: thisMorphSoonFullBounds) ].
						thisMorph fullyCoveredRectangle ifNotNil: [ :fullyCovered |
							r areasOutside: fullyCovered do: [ :notCovered | nonCoveredParts add: notCovered ].
							uncoveredDamage at: j put: nil ]]]].
			uncoveredDamage addAll: nonCoveredParts.
			rootMorphsDamage at: i put: thisMorphDamage ]].

	uncoveredDamage := uncoveredDamage select: [ :r | r notNil ].
	^ uncoveredDamage! !

!methodRemoval: SystemWindow #privateImminentFullyCoveredRect stamp: 'jmv 3/20/2023 17:32:02'!
SystemWindow removeSelector: #privateImminentFullyCoveredRect!
!methodRemoval: Morph #morphLocalBounds stamp: 'jmv 3/20/2023 17:33:43'!
Morph removeSelector: #morphLocalBounds!
!methodRemoval: Morph #privateImminentFullyCoveredRect stamp: 'jmv 3/20/2023 17:32:01'!
Morph removeSelector: #privateImminentFullyCoveredRect!
!methodRemoval: Morph #privateImminentFullBounds stamp: 'jmv 3/20/2023 17:27:37'!
Morph removeSelector: #privateImminentFullBounds!
