'From Cuis 6.0 [latest update: #5705] on 20 March 2023 at 8:12:28 am'!

!Morph methodsFor: 'private' stamp: 'jmv 3/20/2023 07:57:12'!
privateFullyCoveredRect
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	Subclasses may implement smarter behavior, allowing for optimizations of update operations.
	The answer should be the largest fully covered rectangle we can know cheaply.
	
	Note: This method must answer a rectangle that will be covered in the upcoming #drawOn,
	because this method is to be used to direct Display updates.
	Therefore it must NOT use privateDisplayBounds or any other data cached during draw."

	^nil! !


!SystemWindow methodsFor: 'private' stamp: 'jmv 3/20/2023 08:00:18'!
privateFullyCoveredRect
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


!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/20/2023 08:03:07'!
on: paramRectangle fullyCoveredRect: fullyCoveredRectBlock partiallyCoveredPartsDo: partiallyCoveredRectsBlock uncoveredRectsDo: uncoveredRectsBlock
	"Find out about aRectangle and us.
	- If paramRectangle contains a rectangle that is fully covered by us, evaluate fullyCoveredRectBlock with it.
	- For parts of paramRectangle we are certain we won't draw on, evaluate uncoveredRectsBlock
	- The rest of paramRectangle comprises areas which we may cover partially, or can't really be certain
		 Evaluate partiallyCoveredRectsBlock on them.
	Note: The found rectangles, passed to the various blocks, form a partition of paramRectangle.
		This means that all parts of paramRectangle are sent to some block. It also means that no part
		is sent to two different blocks.
	Note: The rectangles passed to argument blocks are new instances. It is OK to modify them as needed.
	Note: if fullyCoveredRectBlock is evaluated, it must be evaluated before any of the other blocks. Senders rely on this!!
	This implementation is a good optimization for many common cases."

	| displayBounds intersectionWithBounds fullyCovered intersectionWithFullyCovered |
	"These are the bounds we'll store in privateDisplayBounds once we #drawOn:, this is about to happen."
	displayBounds := (self externalizeBoundsToWorld: self morphLocalBounds) encompassingIntegerRectangle.	

	"No intersection? Safe answer."
	(paramRectangle intersects: displayBounds) ifFalse: [
		uncoveredRectsBlock value: paramRectangle copy.
		^self ].

	"So, we have some intersection. Use it."
	intersectionWithBounds := paramRectangle intersect: displayBounds.
	fullyCovered := self privateFullyCoveredRect.

	"No part is fully covered. It is just partially covered."
	(fullyCovered isNil or: [ (intersectionWithBounds intersects: fullyCovered) not ]) ifTrue: [
		partiallyCoveredRectsBlock value: intersectionWithBounds.
		paramRectangle areasOutside: intersectionWithBounds do: [ :rect |
			uncoveredRectsBlock value: rect ].
		^self ].

	"So we have some fully covered part."
	intersectionWithFullyCovered := intersectionWithBounds intersect: fullyCovered.
	fullyCoveredRectBlock value: intersectionWithFullyCovered.
	intersectionWithBounds areasOutside: intersectionWithFullyCovered do: [ :rect |
		partiallyCoveredRectsBlock value: rect ].
	paramRectangle areasOutside: intersectionWithBounds do: [ :rect |
		uncoveredRectsBlock value: rect ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/20/2023 08:01:16'!
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
				(rootMorphs at: j) privateFullyCoveredRect ifNotNil: [ :coveringR |
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

!methodRemoval: SystemWindow #fullyCoveredRectangle stamp: 'jmv 3/20/2023 08:11:55'!
SystemWindow removeSelector: #fullyCoveredRectangle!
!methodRemoval: Morph #fullyCovers: stamp: 'jmv 3/20/2023 08:11:55'!
Morph removeSelector: #fullyCovers:!
!methodRemoval: Morph #fullyCoveredRectangle stamp: 'jmv 3/20/2023 08:11:55'!
Morph removeSelector: #fullyCoveredRectangle!
