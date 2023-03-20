'From Cuis 6.0 [latest update: #5706] on 20 March 2023 at 11:55:57 am'!

!BoxedMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Instances may have a border, see instanceVariables borderWidth and borderColor. Subclasses can use a variety of border styles: simple, inset, raised
Subclasses can add things like 'roundedCornerRadius' or such.
Note: Answers false to #submorphsMightProtrude. If you need a box like morph that can hold submorphs that can protrude if (i.e. submorphs may extend beyond box bounds) DONT subclass from here.!


!Morph methodsFor: 'geometry' stamp: 'jmv 3/20/2023 11:24:10'!
morphLocalBounds
	"In our own coordinates!!
	nil if unknown."

	^ nil! !

!Morph methodsFor: 'private' stamp: 'jmv 3/20/2023 11:25:15'!
privateImminentFullBounds
	"Answer a Rectangle whe are certain is to be affected by us and/or submorphs.
	The answer should be the largest affected rectangle we can know cheaply.
	Answer nil if we can't be certain about a better answer.
	
	Note: This method must answer a rectangle that will be affected in the upcoming #drawOn,
	because this method is to be used to direct Display updates.
	Therefore it must NOT use privateDisplayBounds or any other data cached during draw."

	self submorphsMightProtrude ifTrue: [
		^nil ].
	self knowsOwnLocalBounds ifFalse: [
		^nil ].
	self color mightBeTranslucent ifTrue: [
		^ nil ].
	
	"These are the bounds we'll store in privateDisplayBounds once we #drawOn:, this is about to happen.
	outsetBy: 1 for possible anti aliasing."
	^ (self externalizeBoundsToWorld: self morphLocalBounds) encompassingIntegerRectangle
		outsetBy: 1.! !

!Morph methodsFor: 'private' stamp: 'jmv 3/20/2023 11:11:12'!
privateImminentFullyCoveredRect
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	Subclasses may implement smarter behavior, allowing for optimizations of update operations.
	The answer should be the largest fully covered rectangle we can know cheaply.
	
	Note: This method must answer a rectangle that will be covered in the upcoming #drawOn,
	because this method is to be used to direct Display updates.
	Therefore it must NOT use privateDisplayBounds or any other data cached during draw."

	^nil! !


!SystemWindow methodsFor: 'private' stamp: 'jmv 3/20/2023 11:11:16'!
privateImminentFullyCoveredRect
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is a good optimization for many common cases.
	
	Note: This method must answer a rectangle that will be covered in the upcoming #drawOn,
	because this method is to be used to direct Display updates.
	Therefore it must NOT use privateDisplayBounds or any other data cached during draw."

	| b e |
	"If we can't know better."
	self isOrAnyOwnerIsRotated ifTrue: [
		^ nil ].
	color mightBeTranslucent ifTrue: [
		^ nil ].
	"These are the bounds we'll store in privateDisplayBounds once we #drawOn:, this is about to happen."
	b := (self externalizeBoundsToWorld: self morphLocalBounds) encompassingIntegerRectangle.

	e := Theme current roundWindowCorners
		ifFalse: [
			"Due to possible anti aliasing, and possible conversion to integer, we can't
			really be sure 	about the 2 outer pixels at each edge."
			`2@2` ]
		ifTrue: [
			"Add 2 pixels in y direction for anti aliasing and rounding."
			(self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling+2 ].
	^ b insetBy: e.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/20/2023 11:55:04'!
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
				(rootMorphs at: j) privateImminentFullyCoveredRect ifNotNil: [ :coveringR |
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
		thisMorph privateImminentFullBounds ifNotNil: [ :thisMorphSoonFullBounds |
			nonCoveredParts removeAll.
			1 to: uncoveredDamage size do: [ :j |
				(uncoveredDamage at: j) ifNotNil: [ :r |
					(r intersects: thisMorphSoonFullBounds) ifTrue: [
						thisMorphDamage
							ifNil: [ thisMorphDamage := r intersect: thisMorphSoonFullBounds ]
							ifNotNil: [ thisMorphDamage updateMerging: (r intersect: thisMorphSoonFullBounds) ].
						thisMorph privateImminentFullyCoveredRect ifNotNil: [ :fullyCovered |
							r areasOutside: fullyCovered do: [ :notCovered | nonCoveredParts add: notCovered ].
							uncoveredDamage at: j put: nil ]]]].
			uncoveredDamage addAll: nonCoveredParts.
			rootMorphsDamage at: i put: thisMorphDamage ]].

	uncoveredDamage := uncoveredDamage select: [ :r | r notNil ].
	^ uncoveredDamage! !

!methodRemoval: SystemWindow #on:fullyCoveredRect:partiallyCoveredPartsDo:uncoveredRectsDo: stamp: 'jmv 3/20/2023 11:55:23'!
SystemWindow removeSelector: #on:fullyCoveredRect:partiallyCoveredPartsDo:uncoveredRectsDo:!
!methodRemoval: SystemWindow #privateFullyCoveredRect stamp: 'jmv 3/20/2023 11:41:05'!
SystemWindow removeSelector: #privateFullyCoveredRect!
!methodRemoval: Morph #privateFullyCoveredRect stamp: 'jmv 3/20/2023 11:41:05'!
Morph removeSelector: #privateFullyCoveredRect!
!methodRemoval: Morph #on:fullyCoveredRect:partiallyCoveredPartsDo:uncoveredRectsDo: stamp: 'jmv 3/20/2023 11:55:29'!
Morph removeSelector: #on:fullyCoveredRect:partiallyCoveredPartsDo:uncoveredRectsDo:!
