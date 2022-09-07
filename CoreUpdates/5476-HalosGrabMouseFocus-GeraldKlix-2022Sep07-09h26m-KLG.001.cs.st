'From Cuis 6.0 [latest update: #5473] on 7 September 2022 at 9:28:13 am'!

!HaloMorph methodsFor: 'event handling' stamp: 'KLG 9/7/2022 09:27:35'!
popUpFor: aMorph handPosition: handPosition hand: hand

	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ handPosition - aMorph morphPositionInWorld.
	hand newMouseFocus: self.! !

