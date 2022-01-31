'From Cuis 6.0 [latest update: #5053] on 31 January 2022 at 1:56:49 pm'!

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'len 1/31/2022 13:55:54'!
updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.! !

