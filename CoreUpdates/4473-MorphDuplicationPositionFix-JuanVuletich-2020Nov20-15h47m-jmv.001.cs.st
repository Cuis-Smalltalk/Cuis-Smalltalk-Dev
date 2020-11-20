'From Cuis 5.0 [latest update: #4472] on 20 November 2020 at 3:47:57 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/20/2020 15:47:38'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	| hand positionInHandCoordinates |
	hand _ evt hand.
	positionInHandCoordinates _ target morphPositionInWorld - hand morphPositionInWorld.
	"Duplicate has no meaningful position, as it is not in the world. Grab position from original!!"
	target _ target duplicateMorph: evt.
	self addNameString: (target printStringLimitedTo: 40).	
	hand
		obtainHalo: self;
		grabMorph: target delta: positionInHandCoordinates! !

