'From Cuis7.5 [latest update: #7688] on 4 November 2025 at 1:21:25 pm'!

!Morph methodsFor: 'meta-actions' stamp: 'jmv 11/4/2025 13:20:43'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld fb |
	owner ifNil:[^#()].
	myWorld := owner world ifNil: [^#()].
	myRect := self displayFullBounds.
	^Array streamContents: [ :strm |
		myWorld allMorphsBut: self do: [ :m |
			(m isReallyVisible
				and: [ m isReallyUnlocked
					and: [
						fb := m displayFullBounds.
						(fb notNil and: [fb intersects: myRect])
							and: [(#(HaloMorph HaloHandleMorph) statePointsTo: m class name) not]]])
		 ifTrue: [ strm nextPut: m ]]].! !

