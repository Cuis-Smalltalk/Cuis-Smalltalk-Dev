'From Cuis 5.0 [latest update: #4768] on 19 August 2021 at 10:40:15 am'!

!Form methodsFor: 'coloring' stamp: 'jmv 8/19/2021 10:35:00'!
reverse
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white.
	Display reverse. Display forceToScreen.
	"

	(BitBlt toForm: self)
		combinationRule: `Form reverse`;
		copyBits.! !

