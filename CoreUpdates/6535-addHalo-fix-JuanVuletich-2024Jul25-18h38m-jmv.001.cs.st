'From Cuis7.1 [latest update: #6534] on 25 July 2024 at 6:38:36 pm'!

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 7/25/2024 18:33:53'!
popUpFor: aMorph handPosition: handPosition hand: hand

	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset := handPosition - aMorph morphPositionInWorld.! !

