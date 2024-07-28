'From Cuis7.1 [latest update: #6539] on 27 July 2024 at 10:22:39 pm'!

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 7/25/2024 18:33:53'!
popUpFor: aMorph handPosition: handPosition hand: hand

	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset := handPosition - aMorph morphPositionInWorld.! !

