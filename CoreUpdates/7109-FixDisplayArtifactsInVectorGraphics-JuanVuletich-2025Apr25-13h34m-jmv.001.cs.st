'From Cuis7.3 [latest update: #7148] on 25 April 2025 at 1:47:24 pm'!

!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 4/25/2025 13:43:45'!
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
	b := b insetBy: `2@2 corner: 1@1`.
	Theme current roundWindowCorners ifFalse: [
		^ b ].
	e := (self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling.
	^ b origin + e corner: b corner! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 4/25/2025 13:34:33'!
gatherMorphDamage: rootMorphs rootsReportedDamage: rootsReportedDamage addDamageTo: allDamage
	"For each morph, remove any reported damage that is completely covered by another morph on top.
	Add any  areas that need to be redrawn to uncoveredDamage.
	uncoveredDamage will contain all the areas that need to be repaired. rootMorphsReportedDamage no longer relevant.
	Iterate from front to back"

	1 to: rootMorphs size do: [ :i | | thisMorphReportedDamage |
		thisMorphReportedDamage := rootsReportedDamage at: i.
		thisMorphReportedDamage ifNotNil: [
			"Iterate all morphs up to the one directly above us"
			1 to: i-1 do: [ :j |
				(self fullyCoveredRectangleOf: (rootMorphs at: j)) ifNotNil: [ :coveringR | | nonCoveredParts |
					nonCoveredParts := nil.
					1 to: thisMorphReportedDamage size do: [ :k |
						(thisMorphReportedDamage at: k) ifNotNil: [ :r |
							"If damage is completely covered, remove it. If it is partially covered, just keep uncovered parts."
							(coveringR intersects: r) ifTrue: [
								thisMorphReportedDamage at: k put: nil.
								(coveringR containsRect: r) ifFalse: [
									nonCoveredParts :=nonCoveredParts
										ifNil: [ r areasOutside: coveringR ]
										ifNotNil: [ nonCoveredParts, (r areasOutside: coveringR) ]]]]].
					nonCoveredParts ifNotNil: [
						thisMorphReportedDamage addAll: nonCoveredParts ]]].
			"Store"
			thisMorphReportedDamage do: [ :r |
				r ifNotNil: [
					"DebuggingAid"
					"Display fill: r fillColor: Color random. (Delay forSeconds: 0.01) wait."
					allDamage add: r ]]]].

	"Remove redundant rectangles"
	1 to: allDamage size do: [ :i1 |
		(allDamage at: i1) ifNotNil: [ :r1 |
			1 to: allDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(allDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ 
								allDamage at: i2 put: nil ]]]]]].! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 4/25/2025 13:44:53'!
boundingRectOfCurrentIfTrivial
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	currentMorph knowsOwnLocalBounds ifTrue: [
		"Include an extra pixel to cover possible anti aliasing."
		^(currentTransformation externalBoundingRectOf: currentMorph localBounds)
			encompassingIntegerRectangle outsetBy: `0@0 corner: 1@1` ].
	^nil.! !

