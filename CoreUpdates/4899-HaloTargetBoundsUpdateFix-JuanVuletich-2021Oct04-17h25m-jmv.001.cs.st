'From Cuis 5.0 [latest update: #4898] on 4 October 2021 at 5:26:12 pm'!

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/4/2021 17:22:34'!
updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder

	aWorldMorph haloMorphsDo: [ :halo |
		(halo target isRedrawNeeded or: [halo target isSubmorphRedrawNeeded]) ifTrue: [
			"Invalidation of halos requires this specific sequence:"
			halo redrawNeeded.												"invalidate old halo bounds"
			"self fullAddRedrawRect: halo target to: aDamageRecorder." 								"recompute & invalidate target bounds"
			self fullAddRedrawRect: halo to: aDamageRecorder ]].												"recompute & invalidate halo bounds"
	"bogus iteration on halos and targets below is harmless:
		Both now marked as neither #isRedrawNeeded nor #isSubmorphRedrawNeeded."

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.! !

