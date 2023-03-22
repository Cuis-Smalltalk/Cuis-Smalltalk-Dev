'From Cuis 6.0 [latest update: #5709] on 21 March 2023 at 12:38:57 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/21/2023 12:15:26'!
fullyCoveredRectangleOf: aMorph

	^aMorph fullyCoveredRectangle! !


!AbstractVectorCanvas methodsFor: 'morphic' stamp: 'jmv 3/21/2023 12:15:32'!
fullyCoveredRectangleOf: aMorph

	"Assume a bit of Anti Aliasing"
	^aMorph fullyCoveredRectangle ifNotNil: [ :r | r insetBy: 2 ]! !


!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 3/21/2023 12:36:51'!
fullyCoveredRectangleOf: aMorph

	"Assume a bit of Anti Aliasing"
	aMorph requiresVectorCanvas ifTrue: [
		^aMorph fullyCoveredRectangle ifNotNil: [ :r | r insetBy: 2 ]].

	^aMorph fullyCoveredRectangle! !


!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/21/2023 12:13:13'!
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
	Theme current roundWindowCorners ifFalse: [
		^ b ].
	e := (self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling.
	^ b insetBy: e.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/21/2023 12:09:40'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restriction. See inherintance."

	| boundingRect |
	boundingRect := (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
		encompassingIntegerRectangle.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/21/2023 12:05:42'!
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
		thisMorphDamage := nil.
		thisMorph displayFullBounds ifNotNil: [ :thisMorphSoonFullBounds |
			nonCoveredParts removeAll.
			1 to: uncoveredDamage size do: [ :j |
				(uncoveredDamage at: j) ifNotNil: [ :r |
					(r intersects: thisMorphSoonFullBounds) ifTrue: [
						thisMorphDamage
							ifNil: [ thisMorphDamage := r intersect: thisMorphSoonFullBounds ]
							ifNotNil: [ thisMorphDamage updateMerging: (r intersect: thisMorphSoonFullBounds) ].
						(self fullyCoveredRectangleOf: thisMorph) ifNotNil: [ :fullyCovered |
							r areasOutside: fullyCovered do: [ :notCovered | nonCoveredParts add: notCovered ].
							uncoveredDamage at: j put: nil ]]]].
			uncoveredDamage addAll: nonCoveredParts.
			"thisMorphDamage ifNotNil: [ :r |
				Display fill: r fillColor: Color random. (Delay forSeconds: 0.01) wait ]."
			rootMorphsDamage at: i put: thisMorphDamage ]].

	uncoveredDamage := uncoveredDamage select: [ :r | r notNil ].
	^ uncoveredDamage! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 3/21/2023 11:42:15'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid _after_ drawing currentMorph.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used only as an optimization of the area to be redrawn."

	| boundingRect |
	"A good optimization in many cases.
	But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
	for example) this is essential."
	currentMorph knowsOwnLocalBounds ifTrue: [
		boundingRect := (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
			encompassingIntegerRectangle.
		"Include an extra pixel to cover possible anti aliasing."
		boundingRect := boundingRect outsetBy: 1.
		^ clippingMorphDisplayBounds
			ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
			ifNil: [ boundingRect ]].

	"Use the bounds bound by the engine during #drawOn.
	This method must therefore be called after calling #drawOn:."
	boundingRect := engine morphBoundsAfterDraw.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

