'From Cuis 5.0 [latest update: #4552] on 5 April 2021 at 1:02:00 pm'!

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 4/5/2021 12:56:17'!
popUpFor: aMorph handPosition: handPosition hand: hand

	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ handPosition - aMorph morphPositionInWorld! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/5/2021 12:59:49'!
addHalo: aMorphicEvent
	"Defer execution until next cycle, so if you evaluate
	BrowserWindow openBrowser addHalo
	the window is in the world and with proper bounds, before adding the halo.
	"

	| hand position |
	aMorphicEvent
		ifNil: [
			hand _ self world activeHand.
			hand ifNil: [ hand _ self world firstHand ]. 
			position _ hand lastMouseEvent eventPosition ]
		ifNotNil: [
			hand _ aMorphicEvent hand.
			position _ aMorphicEvent eventPosition ].

	self whenUIinSafeState: [
		HaloMorph new popUpFor: self handPosition: position hand: hand
	]! !

!methodRemoval: HaloMorph #popUpFor:event: stamp: 'jmv 4/5/2021 12:59:28'!
HaloMorph removeSelector: #popUpFor:event:!
