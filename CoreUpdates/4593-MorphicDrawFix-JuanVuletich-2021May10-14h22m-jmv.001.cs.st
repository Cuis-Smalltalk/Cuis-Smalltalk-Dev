'From Cuis 5.0 [latest update: #4589] on 10 May 2021 at 2:22:25 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/10/2021 14:22:14'!
updateCurrentDisplayBoundsAndMerge: aRectangle
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	| isKnownFailing r |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph drawOn: self ].
	currentMorph displayBoundsSetFrom: self.
	r _ self boundingRectOfCurrentMorphAfterDraw.
	r _ r quickMerge: aRectangle.
	self clippingByCurrentMorphDo: [
		currentMorph submorphsReverseDo: [ :m |
			r _ self updateDisplayBounds: m andMerge: r ]].
	^r! !

