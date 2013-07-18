'From Cuis 4.1 of 12 December 2012 [latest update: #1559] on 15 January 2013 at 6:01:18 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 1/10/2013 15:55'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| p |
	p _ aMorph morphPositionInWorld.
	self releaseMouseFocus. "Break focus"
	self addMorphBack: aMorph.
	aMorph morphPositionInWorld: p.
	aMorph justGrabbedFrom: formerOwner.! !

