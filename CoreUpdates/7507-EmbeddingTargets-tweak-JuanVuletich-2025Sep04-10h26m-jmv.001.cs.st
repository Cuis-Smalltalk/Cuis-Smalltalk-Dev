'From Cuis7.5 [latest update: #7505] on 4 September 2025 at 10:32:00 am'!

!Morph methodsFor: 'testing' stamp: 'jmv 9/4/2025 10:30:44'!
isReallyUnlocked
	"Answer true only if none in the owner chain is locked (i.e. if we are really unlocked!!)"
	^self isLocked not and: [ owner isReallyUnlocked ]! !


!WorldMorph methodsFor: 'testing' stamp: 'jmv 9/4/2025 10:30:48'!
isReallyUnlocked
	"Answer true only if none in the owner chain is locked (i.e. if we are really unlocked!!)"
	^self isLocked not! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 9/4/2025 10:31:05'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld |
	owner ifNil:[^#()].
	myWorld := owner world ifNil: [^#()].
	myRect := self displayFullBounds.
	^Array streamContents: [ :strm |
		myWorld allMorphsBut: self do: [ :m |
			(m isReallyVisible
			and: [ m isReallyUnlocked
				and: [(m displayFullBounds intersects: myRect)
					and: [(m ~= self)
						and: [(#(HaloMorph HaloHandleMorph) statePointsTo: m class name) not]]]])
		 ifTrue: [ strm nextPut: m ]]].! !

!methodRemoval: PasteUpMorph #isReallyVisible stamp: 'jmv 9/4/2025 10:29:26'!
PasteUpMorph removeSelector: #isReallyVisible!
