'From Cuis 5.0 [latest update: #4552] on 5 April 2021 at 12:40:13 pm'!

!Morph methodsFor: 'events' stamp: 'jmv 4/5/2021 12:39:04'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition

	self addHalo: aMouseButtonEvent.! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/5/2021 12:39:28'!
addHalo: evt
	"Defer execution until next cycle, so if you evaluate
	BrowserWindow openBrowser addHalo
	the window is in the world and with proper bounds, before adding the halo.
	"
	| halo |
	self whenUIinSafeState: [
		self displayBoundsForHalo ifNotNil: [ :r |
			halo _ HaloMorph new.
			halo popUpFor: self event: evt.
			halo morphBounds: r ].
		self redrawNeeded].! !

