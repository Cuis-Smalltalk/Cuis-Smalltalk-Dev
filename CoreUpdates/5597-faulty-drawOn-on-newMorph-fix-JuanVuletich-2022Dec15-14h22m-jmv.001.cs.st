'From Cuis 6.0 [latest update: #5595] on 15 December 2022 at 2:31:32 pm'!

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 12/15/2022 14:31:07'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		[self fullUpdateCurrentProtrudingBounds] on: Error, Halt do: [ :ex |
			self currentMorphDrawingFails.
			ex retry ].
		self outOfMorph ].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 12/15/2022 14:31:24'!
fullUpdateProtrudingBounds: aMorph
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		[self fullUpdateCurrentProtrudingBounds] on: Error, Halt do: [ :ex |
			self currentMorphDrawingFails.
			ex retry ].
		self outOfMorph ].! !

