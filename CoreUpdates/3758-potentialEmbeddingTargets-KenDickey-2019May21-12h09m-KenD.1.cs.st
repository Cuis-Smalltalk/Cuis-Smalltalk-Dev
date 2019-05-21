'From Cuis 5.0 of 7 November 2016 [latest update: #3757] on 21 May 2019 at 12:10:25 pm'!

!Morph methodsFor: 'meta-actions' stamp: 'KenD 5/8/2019 20:51:33'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld |
	owner ifNil:[^#()].
	myWorld := owner world ifNil:[^#()].
	myRect := self morphBoundsInWorld.
	^myWorld submorphs select: [ :m |
		m isReallyVisible
		and: [ m isLocked not
			and: [(m morphBoundsInWorld intersects: myRect)
				and: [(m ~= self)
					and: [(m isKindOf: HaloMorph) not]]]]
		]! !

