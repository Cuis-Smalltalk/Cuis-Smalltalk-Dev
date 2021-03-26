'From Cuis 5.0 [latest update: #4547] on 26 March 2021 at 4:09:31 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'ar 11/7/1999 18:57'!
addHalo
	"Invoke a halo programatically (e.g., not from a meta gesture)"
	^self addHalo: nil! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 3/26/2021 16:09:26'!
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
		self redrawNeeded].
	^halo! !

