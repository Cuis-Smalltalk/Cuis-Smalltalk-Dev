'From Cuis 5.0 [latest update: #4482] on 30 November 2020 at 12:38:38 pm'!

!Morph methodsFor: 'meta-actions' stamp: 'jmv 11/27/2020 17:58:00'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld |
	owner ifNil:[^#()].
	myWorld := owner world ifNil: [^#()].
	myRect := self displayBounds.
	^Array streamContents: [ :strm |
		myWorld allMorphsBut: self do: [ :m |
			(m isReallyVisible
			and: [ m isLocked not
				and: [(m displayBounds intersects: myRect)
					and: [(m ~= self)
						and: [(#(HaloMorph HaloHandleMorph) statePointsTo: m class name) not]]]])
		 ifTrue: [ strm nextPut: m ]]].! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/27/2020 18:04:44'!
allMorphsBut: aMorph do: aBlock 
	"Evaluate the given block for all morphs in this composite morph (including the receiver),
	but avoid aMorph and all its submorphs.
	Also avoid halos (might happen when called on the World) and Hands"

	self == aMorph ifTrue: [ ^self ].
	(self is: #HaloMorph) ifTrue: [ ^self ].
	submorphs do: [:m | m allMorphsBut: aMorph do: aBlock ].
	aBlock value: self.! !

!methodRemoval: WorldMorph #allMorphsBut:do: stamp: 'jmv 11/30/2020 12:38:27'!
WorldMorph removeSelector: #allMorphsBut:do:!
