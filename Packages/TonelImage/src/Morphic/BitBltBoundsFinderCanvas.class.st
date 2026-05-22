Class {
	#name : 'BitBltBoundsFinderCanvas',
	#superclass : 'MorphicCanvas',
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> displayBoundsForHand: aHand [

	^ aHand morphPosition asIntegerPoint - 8 extent: aHand morphExtent.
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> fullAddCurrentMorphTo: aDamageRecorder [

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		currentMorph displayBounds ifNotNil: [ :r | | wb |
			wb := world localBounds.
			(wb intersects: r) ifTrue: [
				aDamageRecorder
					recordInvalidRect: (r intersect: wb)
					for: currentMorph ]]].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> fullAddRedrawRect: aMorph to: aDamageRecorder [
	
	aMorph visible ifTrue: [
		(aMorph isRedrawNeeded or: [aMorph isSubmorphRedrawNeeded]) ifTrue: [
			self into: aMorph.
			self fullAddCurrentMorphTo: aDamageRecorder.
			self outOfMorph
		]]
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> fullUpdateCurrentProtrudingBounds [
	"Recursively update display bounds, for currentMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."

	self displayBoundsSetOnCurrentMorph.
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> fullUpdateProtrudingBounds: aMorph [
	"Recursively update display bounds, for aMorph and all submorphs,
	but only for those whose bounds may go outside owner bounds."
	
	aMorph visible ifTrue: [
		self into: aMorph.
		self fullUpdateCurrentProtrudingBounds.
		self outOfMorph ].
]

{ #category : 'testing' }
BitBltBoundsFinderCanvas >> isBoundsFinderCanvas [
	^true
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder [

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> updateCurrentBounds [
	"Update display bounds, for currentMorph only."

	self displayBoundsSetOnCurrentMorph.
]

{ #category : 'morphic' }
BitBltBoundsFinderCanvas >> updateHandsDisplayBounds: aWorldMorph [
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."

	| hand |
	hand := aWorldMorph activeHand.
	hand hasSubmorphs ifTrue: [
		self into: hand.
		hand submorphsDo: [ :morph |
			morph basicDisplayBounds ifNil: [
				self fullUpdateProtrudingBounds: morph ]].
		self outOfMorph ].
]
