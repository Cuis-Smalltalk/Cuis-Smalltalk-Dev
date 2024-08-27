'From Cuis7.1 [latest update: #6642] on 27 August 2024 at 2:50:03 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/27/2024 14:14:05'!
addHalo: aMorphicEventOrNil

	| hand position |
	aMorphicEventOrNil
		ifNil: [
			hand := self world activeHand.
			position := hand lastMouseEvent eventPosition ]
		ifNotNil: [
			hand := aMorphicEventOrNil hand.
			position := aMorphicEventOrNil eventPosition ].

	HaloMorph new popUpFor: self handPosition: position hand: hand.! !

!methodRemoval: WorldMorph #firstHand stamp: 'jmv 8/27/2024 14:49:51'!
WorldMorph removeSelector: #firstHand!
