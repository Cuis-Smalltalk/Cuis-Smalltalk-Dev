'From Cuis 5.0 [latest update: #4552] on 5 April 2021 at 12:32:57 pm'!

!HaloMorph methodsFor: 'events' stamp: 'jmv 4/5/2021 12:32:51'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil: [
		self delete.
		^self].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset _ aMouseButtonEvent eventPosition - target morphPositionInWorld.
	"wait for click to transfer halo"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:localPosition:
		dblClkSel: nil! !

