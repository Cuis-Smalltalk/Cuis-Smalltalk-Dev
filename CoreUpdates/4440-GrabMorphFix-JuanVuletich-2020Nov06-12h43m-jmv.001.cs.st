'From Cuis 5.0 [latest update: #4438] on 6 November 2020 at 12:43:41 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 11/6/2020 12:38:36'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		h target == aMorph ifTrue: [
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
		d _ aMorph morphPositionInWorld - formerPositionInWorld.
		d isZero ifFalse: [
			aMorph displayBounds: (r translatedBy: d)]].
	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		lastPosition _ self morphPosition ].! !

