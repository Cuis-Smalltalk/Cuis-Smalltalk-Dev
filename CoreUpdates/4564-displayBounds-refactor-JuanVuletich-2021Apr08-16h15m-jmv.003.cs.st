'From Cuis 5.0 [latest update: #4562] on 8 April 2021 at 5:14:06 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 4/8/2021 16:56:40'!
updateCurrentDisplayBoundsAndMerge: aRectangle
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	| r |
	currentMorph drawOn: self.
	currentMorph displayBoundsSetFrom: self.
	r _ self boundingRectOfCurrentMorphAfterDraw.
	r _ r quickMerge: aRectangle.
	self clippingByCurrentMorphDo: [
		currentMorph submorphsReverseDo: [ :m |
			r _ self updateDisplayBounds: m andMerge: r ]].
	^r! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 4/8/2021 16:56:43'!
updateDisplayBounds: aMorph andMerge: aRectangle
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."

	| r |
	r _ aRectangle.
	aMorph visible ifTrue: [
		self into: aMorph.
		r _ self canvasToUse updateCurrentDisplayBoundsAndMerge: r.
		self outOfMorph
		].
	^r! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 4/8/2021 17:13:57'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				morph displayBounds ifNil: [
					self updateDisplayBounds: morph andMerge: nil ]].
			self outOfMorph ]]! !

!methodRemoval: MorphicCanvas #updateCurrentDisplayBounds stamp: 'jmv 4/8/2021 16:58:38'!
MorphicCanvas removeSelector: #updateCurrentDisplayBounds!
!methodRemoval: MorphicCanvas #updateDisplayBounds: stamp: 'jmv 4/8/2021 16:58:40'!
MorphicCanvas removeSelector: #updateDisplayBounds:!
