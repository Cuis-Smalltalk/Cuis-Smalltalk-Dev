'From Cuis 5.0 [latest update: #4789] on 23 August 2021 at 12:15:23 pm'!

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/23/2021 11:15:37'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		currentMorph submorphsReverseDo: [ :m |
			self fullAddRedrawRect: m to: aDamageRecorder ]].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/23/2021 11:21:37'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	
	aMorph visible ifTrue: [
		(aMorph isRedrawNeeded or: [aMorph isSubmorphRedrawNeeded]) ifTrue: [
			self into: aMorph.
			self fullAddCurrentMorphTo: aDamageRecorder.
			self outOfMorph
		]]! !

!methodRemoval: BitBltBoundsFinderCanvas #fullAddCurrentRect:submorphs:to: stamp: 'jmv 8/23/2021 12:14:48'!
BitBltBoundsFinderCanvas removeSelector: #fullAddCurrentRect:submorphs:to:!
