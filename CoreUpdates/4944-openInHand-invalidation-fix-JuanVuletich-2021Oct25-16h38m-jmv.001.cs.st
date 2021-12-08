'From Cuis 5.0 [latest update: #4943] on 25 October 2021 at 4:39:04 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/25/2021 16:24:56'!
adjustDisplayBoundsBy: delta
	"Private for framework use.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	privateDisplayBounds ifNotNil: [
		privateDisplayBounds _ delta ifNil: [ nil ] ifNotNil: [privateDisplayBounds translatedBy: delta]].
	self allSubmorphsDo: [ :m | m adjustDisplayBoundsBy: delta ].! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/25/2021 16:35:33'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld _ aMorph morphPositionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		d _ formerPositionInWorld ifNotNil: [aMorph morphPositionInWorld - formerPositionInWorld].
		(d isNil or: [d isZero not]) ifTrue: [
			aMorph adjustDisplayBoundsBy: d ]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		"Morph was in the world"
		lastPosition _ self morphPosition ].! !

