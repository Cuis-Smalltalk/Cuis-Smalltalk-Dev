'From Cuis 4.2 of 25 July 2013 [latest update: #2392] on 30 June 2015 at 9:50:37.118103 am'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 6/30/2015 09:50'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| morphPos |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		h target == aMorph ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	morphPos := aMorph morphPositionInWorld.
	grabMorphData
		at: aMorph 
		put: { formerOwner. morphPos. }.
	self addMorphBack: aMorph position: morphPos - self morphPositionInWorld.
	aMorph justGrabbedFrom: formerOwner! !

