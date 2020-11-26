'From Cuis 5.0 [latest update: #4479] on 26 November 2020 at 3:07:55 pm'!

!Morph methodsFor: 'accessing' stamp: 'jmv 11/26/2020 15:05:00'!
location: aGeometryTransformation! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/26/2020 14:34:52'!
allMorphsBut: aMorph do: aBlock 
	"Evaluate the given block for all morphs in this composite morph (including the receiver),
	but avoid aMorph and all its submorphs."

	self == aMorph ifFalse: [
		submorphs do: [:m | m allMorphsBut: aMorph do: aBlock ].
		aBlock value: self ].! !


!MovableMorph methodsFor: 'accessing' stamp: 'jmv 11/26/2020 13:27:14'!
location: aGeometryTransformation
	location _ aGeometryTransformation! !


!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/26/2020 14:34:48'!
allMorphsBut: aMorph do: aBlock 
	"Evaluate the given block for all morphs in this composite morph (including the receiver),
	but avoid aMorph and all its submorphs."

	self == aMorph ifFalse: [
		submorphs do: [:m | m allMorphsBut: aMorph do: aBlock ].
		aBlock value: self.
		self handsReverseDo: [:h | h allMorphsBut: aMorph do: aBlock ]].! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 11/26/2020 15:05:27'!
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
						and: [(m isKindOf: HaloMorph) not]]]]) ifTrue: [ strm nextPut: m ]]]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 11/26/2020 15:06:40'!
addMorphFrontFromWorldPosition: aMorph

	| tx |
	tx _ GeometryTransformation identity.
	aMorph withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].
	self addMorphFront: aMorph.
	aMorph location: tx! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 11/26/2020 15:06:03'!
attachMorphBeside: aMorph

	"Position the given morph beside this hand, then grab it."

	aMorph aboutToBeGrabbedBy: self.
	self grabMorph: aMorph delta: (self morphWidth)@0.
! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 11/26/2020 15:00:56'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].

	moveUnderHand
		ifTrue: [
			"We can possibly do better, especially for non WidgetMorphs"
			positionInHandCoordinates _ -30 @ -10.
			grabbed isInWorld ifTrue: [
				grabbed displayBounds ifNotNil: [ :r |
					positionInHandCoordinates _ (r extent // 2) negated ]].
			self
				grabMorph: grabbed
				delta: positionInHandCoordinates.
			^self ].

	positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		morphPositionInWorld - self morphPositionInWorld.

	tx _ GeometryTransformation identity.
	aMorph withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].

	self
		grabMorph: grabbed
		delta: positionInHandCoordinates.

	aMorph location: tx.! !

