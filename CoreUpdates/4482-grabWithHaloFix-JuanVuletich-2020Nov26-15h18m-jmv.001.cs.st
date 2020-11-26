'From Cuis 5.0 [latest update: #4481] on 26 November 2020 at 3:18:46 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 11/26/2020 15:17:14'!
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
		formerPositionInWorld ifNotNil: [
			d _ aMorph morphPositionInWorld - formerPositionInWorld.
			d isZero ifFalse: [
				aMorph displayBounds: (r translatedBy: d)]]].
	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		lastPosition _ self morphPosition ].! !

