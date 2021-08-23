'From Cuis 5.0 [latest update: #4794] on 23 August 2021 at 2:35:05 pm'!

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/23/2021 14:33:25'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 8/23/2021 14:31:43'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle.
	boundsFinderCanvas ifNotNil: [
		boundsFinderCanvas setClipRect: aRectangle ].! !

