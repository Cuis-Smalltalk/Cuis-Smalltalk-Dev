'From Cuis 5.0 [latest update: #4854] on 9 September 2021 at 2:05:28 pm'!

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 9/9/2021 14:05:20'!
updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder

	aWorldMorph haloMorphsDo: [ :halo |
		(halo target isRedrawNeeded or: [halo target isSubmorphRedrawNeeded]) ifTrue: [
			"Invalidation of halos requires this specific sequence:"
			halo redrawNeeded.												"invalidate old halo bounds"
			self fullAddRedrawRect: halo target to: aDamageRecorder. 								"recompute & invalidate target bounds"
			self fullAddRedrawRect: halo to: aDamageRecorder ]].												"recompute & invalidate halo bounds"
	"bogus iteration on halos and targets below is harmless:
		Both now marked as neither #isRedrawNeeded nor #isSubmorphRedrawNeeded."

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.! !

