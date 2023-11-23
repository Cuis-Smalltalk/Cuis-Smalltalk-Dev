'From Cuis 6.0 [latest update: #6091] on 23 November 2023 at 4:32:34 pm'!

!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 11/23/2023 16:31:27'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		currentMorph displayBounds ifNotNil: [ :r |
			aDamageRecorder recordInvalidRect: r for: currentMorph ]].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 11/23/2023 16:31:46'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		currentMorph displayBounds ifNotNil: [ :r |
			aDamageRecorder recordInvalidRect: r for: currentMorph ]].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !

